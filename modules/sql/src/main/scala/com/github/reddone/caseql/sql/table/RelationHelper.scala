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
    // (SELECT COUNT(*)
    //   FROM (SELECT * FROM rightTable WHERE filter) as rightTable
    //   WHERE rightTable.id = leftTable.id)
    //   =
    //  (SELECT COUNT(*)
    //   FROM rightTable
    //   WHERE rightTable.id = leftTable.id)
    val makeEveryFragment = (filterFragment: Fragment) =>
      const(s"($Select $Count ($Star) $From ${rightQuerySyntax.aliasedName} $Where $joinCondition $And") ++
        filterFragment ++
        const(s") = ($Select $Count ($Star) $From ${rightQuerySyntax.aliasedName} $Where $joinCondition)")
    val every = f.EVERY
      .flatMap(tableFilter.byFilterFragment(_, StringUtils.strToOpt(rightAlias)))
      .map(makeEveryFragment)
    // DIRECT LINK - SOME (contrary of NONE)
    // EXISTS (
    //   SELECT 1
    //   FROM (SELECT * FROM rightTable WHERE filter) as rightTable
    //   WHERE rightTable.id = leftTable.id
    // )
    val makeSomeFragment = (filterFragment: Fragment) =>
      const(s"$Exists ($Select 1 $From ${rightQuerySyntax.aliasedName} $Where $joinCondition $And") ++
        filterFragment ++ const(")")
    val some = f.SOME
      .flatMap(tableFilter.byFilterFragment(_, StringUtils.strToOpt(rightAlias)))
      .map(makeSomeFragment)
    // DIRECT LINK - NONE (contrary of SOME)
    // NOT EXISTS (
    //   SELECT 1
    //   FROM (SELECT * FROM rightTable WHERE filter) as rightTable
    //   WHERE rightTable.id = leftTable.id
    // )
    val makeNoneFragment = (filterFragment: Fragment) =>
      const(s"$NotExists ($Select 1 $From ${rightQuerySyntax.aliasedName} $Where $joinCondition $And") ++
        filterFragment ++ const(")")
    val none = f.NONE
      .flatMap(tableFilter.byFilterFragment(_, StringUtils.strToOpt(rightAlias)))
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
    //   WHERE joinTable.id = leftTable.id AND IS NULL rightTable.id
    // )
    val makeEveryFragment = (filterFragment: Fragment) =>
      const(
        s"$NotExists (" +
          s"$Select 1 $From ${junctionSyntax.aliasedName} $LeftJoin ${rightQuerySyntax.aliasedName} " +
          s"$On ${rightJoinCondition} " +
          s"$Where ${leftJoinCondition} $And ${areNulls(rightQuerySyntax, rightJoinFields.map(_._1))} $And"
      ) ++ filterFragment ++ const(")")
    val every = f.EVERY
      .flatMap(tableFilter.byFilterFragment(_, StringUtils.strToOpt(rightAlias)))
      .map(makeEveryFragment)
    // JUNCTION LINK - SOME (contrary of NONE)
    // (you can use "LEFT JOIN" and add "IS NOT NULL rightTable.id")
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
          s"$Select 1 $From ${junctionSyntax.aliasedName} $InnerJoin ${rightQuerySyntax.aliasedName} " +
          s"$On ${rightJoinCondition} " +
          s"$Where ${leftJoinCondition} $And"
      ) ++ filterFragment ++ const(")")
    val some = f.SOME
      .flatMap(tableFilter.byFilterFragment(_, StringUtils.strToOpt(rightAlias)))
      .map(makeSomeFragment)
    // JUNCTION LINK - NONE (contrary of SOME)
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
          s"$Select 1 $From ${junctionSyntax.aliasedName} $InnerJoin ${rightQuerySyntax.aliasedName} " +
          s"$On ${rightJoinCondition} " +
          s"$Where ${leftJoinCondition} $And"
      ) ++ filterFragment ++ const(")")
    val none = f.NONE
      .flatMap(tableFilter.byFilterFragment(_, StringUtils.strToOpt(rightAlias)))
      .map(makeNoneFragment)
    // AND between EVERY, SOME, NONE
    FragmentUtils.optionalAndOpt(every, some, none)
  }

  private def resolveRightAlias[A, B](leftSyntax: TableSyntax[A], rightSyntax: TableSyntax[B]): String =
    if (leftSyntax.alias == rightSyntax.alias) {
      s"${rightSyntax.alias}_2"
    } else {
      rightSyntax.alias
    }

  private def areNulls[A](syntax: TableSyntax[A], fields: Seq[String]): String = {
    fields
      .map(syntax.aliasedColumn)
      .map(s"$IsNull " + _)
      .mkString(s" $And ")
  }
}
