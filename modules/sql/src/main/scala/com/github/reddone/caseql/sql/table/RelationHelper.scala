package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.filter.wrappers.{EntityFilter, RelationFilter}
import com.github.reddone.caseql.sql.tokens._
import com.github.reddone.caseql.sql.util.{FragmentUtils, StringUtils}
import doobie._
import Fragment._
import shapeless.labelled.FieldType
import shapeless.{::, HNil, Poly1, Witness}

object RelationHelper {

  trait extract[T] extends skip {
    implicit def atType[K <: Symbol, V <: T](
        implicit wt: Witness.Aux[K]
    ): Case.Aux[FieldType[K, V], FieldType[K, V] :: HNil] =
      at[FieldType[K, V]](_ :: HNil)
  }

  trait skip extends Poly1 {
    implicit def atDefault[T]: Case.Aux[T, HNil] = at[T](_ => HNil)
  }

  def processDirectRelationFilter[A, B, FB <: EntityFilter[FB]](
      alias: Option[String],
      leftSyntax: TableSyntax[A],
      rightSyntax: TableSyntax[B],
      joinFields: List[(String, String)],
      tableFilter: TableFilter[B, FB]
  ): RelationFilter[A, B, FB] => Option[Fragment] = f => {
    val leftQuerySyntax  = alias.map(leftSyntax.withAlias).getOrElse(leftSyntax)
    val rightAlias       = resolveRightAlias(leftQuerySyntax, rightSyntax)
    val rightQuerySyntax = rightSyntax.withAlias(rightAlias)
    val joinCondition = joinFields
      .map {
        case (l, r) => s"${leftQuerySyntax.aliasedColumn(l)} = ${rightQuerySyntax.aliasedColumn(r)}"
      }
      .mkString(s" $And ")
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
          s"$From ($Select $Star $From ${rightQuerySyntax.aliasedName} $Where"
      ) ++ filterFragment ++
        const(
          s") $As ${rightQuerySyntax.aliasedName} " +
            s"$Where $joinCondition " +
            s"$Having $Count ($Star) $GreaterThan 0 $And $Count ($Star) $Equal (" +
            s"$Select $Count ($Star) " +
            s"$From ${rightQuerySyntax.aliasedName} " +
            s"$Where $joinCondition ) )"
        )
    val every = f.EVERY
      .flatMap(tableFilter.byFilterFragment(_, StringUtils.strToOpt(rightQuerySyntax.alias)))
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
          s"$From ($Select $Star $From ${rightQuerySyntax.aliasedName} $Where"
      ) ++ filterFragment ++
        const(
          s") $As ${rightQuerySyntax.aliasedName} " +
            s"$Where $joinCondition )"
        )
    val some = f.SOME
      .flatMap(tableFilter.byFilterFragment(_, StringUtils.strToOpt(rightQuerySyntax.alias)))
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
          s"$From ($Select $Star $From ${rightQuerySyntax.aliasedName} $Where"
      ) ++ filterFragment ++
        const(
          s") $As ${rightQuerySyntax.aliasedName} " +
            s"$Where $joinCondition )"
        )
    val none = f.NONE
      .flatMap(tableFilter.byFilterFragment(_, StringUtils.strToOpt(rightQuerySyntax.alias)))
      .map(makeNoneFragment)
    // AND between EVERY, SOME, NONE
    FragmentUtils.optionalAndOpt(every, some, none)
  }

  def processJunctionRelationFilter[A, B, C, FB <: EntityFilter[FB]](
      alias: Option[String],
      leftSyntax: TableSyntax[A],
      rightSyntax: TableSyntax[B],
      junctionSyntax: TableSyntax[C],
      leftJoinFields: List[(String, String)],
      rightJoinFields: List[(String, String)],
      tableFilter: TableFilter[B, FB]
  ): RelationFilter[A, B, FB] => Option[Fragment] = f => {
    val leftQuerySyntax  = alias.map(leftSyntax.withAlias).getOrElse(leftSyntax)
    val rightAlias       = resolveRightAlias(leftQuerySyntax, rightSyntax)
    val rightQuerySyntax = rightSyntax.withAlias(rightAlias)
    val leftJoinCondition = leftJoinFields
      .map {
        case (l, r) => s"${leftQuerySyntax.aliasedColumn(l)} = ${junctionSyntax.aliasedColumn(r)}"
      }
      .mkString(s" $And ")
    val rightJoinCondition = rightJoinFields
      .map {
        case (l, r) => s"${rightQuerySyntax.aliasedColumn(l)} = ${junctionSyntax.aliasedColumn(r)}"
      }
      .mkString(s" $And ")
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
          s"$Select 1 $From ${junctionSyntax.aliasedName} " +
          s"$LeftJoin ($Select $Star $From ${rightQuerySyntax.aliasedName} $Where"
      ) ++ filterFragment ++
        const(
          s") $As ${rightQuerySyntax.aliasedName} " +
            s"$On $rightJoinCondition " +
            s"$Where $leftJoinCondition $And ${areNulls(rightQuerySyntax, rightJoinFields.map(_._1))} )"
        )
    val every = f.EVERY
      .flatMap(tableFilter.byFilterFragment(_, StringUtils.strToOpt(rightQuerySyntax.alias)))
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
          s"$Select 1 $From ${junctionSyntax.aliasedName} " +
          s"$InnerJoin ($Select $Star $From ${rightQuerySyntax.aliasedName} $Where"
      ) ++ filterFragment ++
        const(
          s") $As ${rightQuerySyntax.aliasedName} " +
            s"$On $rightJoinCondition " +
            s"$Where $leftJoinCondition )"
        )
    val some = f.SOME
      .flatMap(tableFilter.byFilterFragment(_, StringUtils.strToOpt(rightQuerySyntax.alias)))
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
          s"$Select 1 $From ${junctionSyntax.aliasedName} " +
          s"$InnerJoin ($Select $Star $From ${rightQuerySyntax.aliasedName} $Where"
      ) ++ filterFragment ++
        const(
          s") $As ${rightQuerySyntax.aliasedName} " +
            s"$On $rightJoinCondition " +
            s"$Where $leftJoinCondition )"
        )
    val none = f.NONE
      .flatMap(tableFilter.byFilterFragment(_, StringUtils.strToOpt(rightQuerySyntax.alias)))
      .map(makeNoneFragment)
    // AND between EVERY, SOME, NONE
    FragmentUtils.optionalAndOpt(every, some, none)
  }

  private def resolveRightAlias[A, B](leftSyntax: TableSyntax[A], rightSyntax: TableSyntax[B]): String =
    if (leftSyntax.aliasedName == rightSyntax.aliasedName) { // it's the same table
      if (rightSyntax.alias.isEmpty) {                       // and right part has no alias
        "self"                                               // use 'self' an alias
      } else {                                               // or if right part has an alias
        s"${rightSyntax.alias}_self"                         // append '_self' to existing alias
      }                                                      // it's not the same table
    } else {                                                 // keep using right alias
      rightSyntax.alias                                      // because it's not a self join
    }

  private def areNulls(syntax: TableSyntax[_], fields: Seq[String]): String = {
    fields.map(syntax.aliasedColumn).map(_ + s" $IsNull").mkString(s" $And ")
  }
}
