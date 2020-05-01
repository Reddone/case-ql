package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.filter.wrappers.{EntityFilter, RelationFilter}
import com.github.reddone.caseql.sql.tokens._
import com.github.reddone.caseql.sql.util.FragmentUtils
import doobie._
import Fragment._

object RelationHelper {

  def processDirectRelationFilter[A, B, FB <: EntityFilter[FB]](
      alias: String,
      leftSyntax: TableSyntax[A],
      rightSyntax: TableSyntax[B],
      joinFields: List[(String, String)],
      tableFilter: TableFilter[B, FB]
  ): RelationFilter[A, B, FB] => Option[Fragment] = f => {
    val leftQuerySyntax = leftSyntax.withAlias(alias)
    val rightQuerySyntax = if (leftQuerySyntax.name == rightSyntax.name) {
      rightSyntax.withAlias(s"${rightSyntax.name}_rel")
    } else {
      rightSyntax
    }
    val joinCondition = joinFields
      .map {
        case (l, r) => s"${leftQuerySyntax.selectionColumn(l)} = ${rightQuerySyntax.selectionColumn(r)}"
      }
      .mkString(s" $And ")
    // SUB QUERY
    // SELECT * FROM rightTable WHERE filter
    val makeSubQueryFragment = (filterFragment: Fragment) =>
      const(s"$Select $Star $From ${rightSyntax.fullName} $Where") ++ filterFragment
    // DIRECT LINK - EVERY
    // EXISTS (
    //   SELECT 1
    //   FROM (SELECT * FROM rightTable WHERE filter) as rightTable
    //   WHERE rightTable.id = leftTable.id
    //   HAVING COUNT(*) > 0 AND COUNT(*) =
    //   SELECT COUNT(*)
    //   FROM rightTable
    //   WHERE rightTable.id = leftTable.id
    // )
    val makeEveryFragment = (filterFragment: Fragment) =>
      const(
        s"$Exists (" +
          s"$Select 1 " +
          s"$From"
      ) ++ Fragments.parentheses(makeSubQueryFragment(filterFragment)) ++
        const(
          s"$As ${rightQuerySyntax.name} " +
            s"$Where $joinCondition " +
            s"$Having $Count ($Star) $GreaterThan 0 $And $Count ($Star) $Equal (" +
            s"$Select $Count ($Star) " +
            s"$From ${rightQuerySyntax.fullName} " +
            s"$Where $joinCondition ) )"
        )
    val every = f.EVERY
      .flatMap(tableFilter.byFilterFragment(_, rightSyntax.alias))
      .map(makeEveryFragment)
    // DIRECT LINK - SOME (negation of NONE)
    // EXISTS (
    //   SELECT 1
    //   FROM (SELECT * FROM rightTable WHERE filter) as rightTable
    //   WHERE rightTable.id = leftTable.id
    // )
    val makeSomeFragment = (filterFragment: Fragment) =>
      const(
        s"$Exists (" +
          s"$Select 1 " +
          s"$From"
      ) ++ Fragments.parentheses(makeSubQueryFragment(filterFragment)) ++
        const(
          s"$As ${rightQuerySyntax.name} " +
            s"$Where $joinCondition )"
        )
    val some = f.SOME
      .flatMap(tableFilter.byFilterFragment(_, rightSyntax.alias))
      .map(makeSomeFragment)
    // DIRECT LINK - NONE (negation of SOME)
    // NOT EXISTS (
    //   SELECT 1
    //   FROM (SELECT * FROM rightTable WHERE filter) as rightTable
    //   WHERE rightTable.id = leftTable.id
    // )
    val makeNoneFragment = (filterFragment: Fragment) =>
      const(
        s"$NotExists (" +
          s"$Select 1 " +
          s"$From"
      ) ++ Fragments.parentheses(makeSubQueryFragment(filterFragment)) ++
        const(
          s"$As ${rightQuerySyntax.name} " +
            s"$Where $joinCondition )"
        )
    val none = f.NONE
      .flatMap(tableFilter.byFilterFragment(_, rightSyntax.alias))
      .map(makeNoneFragment)
    // AND between EVERY, SOME, NONE
    FragmentUtils.optionalAndOpt(every, some, none)
  }

  def processJunctionRelationFilter[A, B, C, FB <: EntityFilter[FB]](
      alias: String,
      leftSyntax: TableSyntax[A],
      rightSyntax: TableSyntax[B],
      junctionSyntax: TableSyntax[C],
      leftJoinFields: List[(String, String)],
      rightJoinFields: List[(String, String)],
      tableFilter: TableFilter[B, FB]
  ): RelationFilter[A, B, FB] => Option[Fragment] = f => {
    val leftQuerySyntax = leftSyntax.withAlias(alias)
    val rightQuerySyntax = if (leftQuerySyntax.name == rightSyntax.name) {
      rightSyntax.withAlias(s"${rightSyntax.name}_rel")
    } else {
      rightSyntax
    }
    val junctionQuerySyntax =
      if (leftQuerySyntax.name == junctionSyntax.name ||
          rightQuerySyntax.name == junctionSyntax.name) {
        junctionSyntax.withAlias(s"${junctionSyntax.name}_jun")
      } else {
        junctionSyntax
      }
    val leftJoinCondition = leftJoinFields
      .map {
        case (l, r) => s"${leftQuerySyntax.selectionColumn(l)} = ${junctionQuerySyntax.selectionColumn(r)}"
      }
      .mkString(s" $And ")
    val rightJoinCondition = rightJoinFields
      .map {
        case (l, r) => s"${rightQuerySyntax.selectionColumn(l)} = ${junctionQuerySyntax.selectionColumn(r)}"
      }
      .mkString(s" $And ")
    // SUB QUERY
    // SELECT * FROM rightTable WHERE filter
    val makeSubQueryFragment = (filterFragment: Fragment) =>
      const(s"$Select $Star $From ${rightSyntax.fullName} $Where") ++ filterFragment
    // JUNCTION LINK - EVERY
    // NOT EXISTS (
    //   SELECT 1
    //   FROM joinTable
    //   LEFT JOIN (SELECT * FROM rightTable WHERE filter) AS rightTable
    //   ON joinTable.id = rightTable.id
    //   WHERE joinTable.id = leftTable.id AND rightTable.id IS NULL
    // )
    val makeEveryFragment = (filterFragment: Fragment) =>
      const(
        s"$NotExists (" +
          s"$Select 1 " +
          s"$From ${junctionQuerySyntax.fullName} " +
          s"$LeftJoin"
      ) ++ Fragments.parentheses(makeSubQueryFragment(filterFragment)) ++
        const(
          s"$As ${rightQuerySyntax.name} " +
            s"$On $rightJoinCondition " +
            s"$Where $leftJoinCondition $And ${areNulls(rightQuerySyntax, rightJoinFields.map(_._1))} )"
        )
    val every = f.EVERY
      .flatMap(tableFilter.byFilterFragment(_, rightSyntax.alias))
      .map(makeEveryFragment)
    // JUNCTION LINK - SOME (negation of NONE)
    // (you can use LEFT JOIN and add "IS NOT NULL rightTable.id")
    // EXISTS (
    //   SELECT 1
    //   FROM joinTable
    //   INNER JOIN (SELECT * FROM rightTable WHERE filter) AS rightTable
    //   ON joinTable.id = rightTable.id
    //   WHERE joinTable.id = leftTable.id
    // )
    val makeSomeFragment = (filterFragment: Fragment) =>
      const(
        s"$Exists (" +
          s"$Select 1 " +
          s"$From ${junctionQuerySyntax.fullName} " +
          s"$InnerJoin"
      ) ++ Fragments.parentheses(makeSubQueryFragment(filterFragment)) ++
        const(
          s"$As ${rightQuerySyntax.name} " +
            s"$On $rightJoinCondition " +
            s"$Where $leftJoinCondition )"
        )
    val some = f.SOME
      .flatMap(tableFilter.byFilterFragment(_, rightSyntax.alias))
      .map(makeSomeFragment)
    // JUNCTION LINK - NONE (negation of SOME)
    // (you can use LEFT JOIN and add "IS NOT NULL rightTable.id")
    // NOT EXISTS (
    //   SELECT 1
    //   FROM joinTable
    //   INNER JOIN (SELECT * FROM rightTable WHERE filter) AS rightTable
    //   ON joinTable.id = rightTable.id
    //   WHERE joinTable.id = leftTable.id
    // )
    val makeNoneFragment = (filterFragment: Fragment) =>
      const(
        s"$NotExists (" +
          s"$Select 1 " +
          s"$From ${junctionQuerySyntax.fullName} " +
          s"$InnerJoin"
      ) ++ Fragments.parentheses(makeSubQueryFragment(filterFragment)) ++
        const(
          s"$As ${rightQuerySyntax.name} " +
            s"$On $rightJoinCondition " +
            s"$Where $leftJoinCondition )"
        )
    val none = f.NONE
      .flatMap(tableFilter.byFilterFragment(_, rightSyntax.alias))
      .map(makeNoneFragment)
    // AND between EVERY, SOME, NONE
    FragmentUtils.optionalAndOpt(every, some, none)
  }

  private def areNulls(syntax: TableSyntax[_], fields: Seq[String]): String = {
    fields.map(syntax.selectionColumn).map(_ + s" $IsNull").mkString(s" $And ")
  }
}
