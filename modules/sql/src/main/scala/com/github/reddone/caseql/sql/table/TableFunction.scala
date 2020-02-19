package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.filter.models.Filter
import com.github.reddone.caseql.sql.filter.wrappers.{EntityFilter, RelationFilter}
import com.github.reddone.caseql.sql.modifier.models.Modifier
import com.github.reddone.caseql.sql.tokens._
import com.github.reddone.caseql.sql.util.{FragmentUtils, StringUtils}
import doobie._
import Fragment._
import shapeless.labelled.{FieldType, field}
import shapeless.{::, HNil, Poly1, Witness}

object TableFunction {

  // type extractors

  object extractFilter extends extract[Option[Filter[_]]]

  object extractModifier extends extract[Option[Modifier[_]]]

  object extractRelationFilter extends extract[Option[RelationFilter[_, _, _]]]

  trait extract[T] extends skip {
    implicit def atType[K <: Symbol, V <: T](
        implicit wt: Witness.Aux[K]
    ): Case.Aux[FieldType[K, V], FieldType[K, V] :: HNil] =
      at[FieldType[K, V]](_ :: HNil)
  }

  trait skip extends Poly1 {
    implicit def atDefault[T]: Case.Aux[T, HNil] = at[T](_ => HNil)
  }

  // type mappers

  object filterToNamedOptionFragment extends Poly1 {
    implicit def atOptionFilter[K <: Symbol, V <: Option[Filter[_]]](
        implicit wt: Witness.Aux[K]
    ): Case.Aux[FieldType[K, V], FieldType[K, (String, String => Option[Fragment])]] =
      at[FieldType[K, V]] { ft =>
        val name = wt.value.name
        field[K](ft.map(f => (name, f.toOptionFragment _)).getOrElse((name, (_: String) => None)))
      }
  }

  object modifierToNamedOptionFragment extends Poly1 {
    implicit def atOptionModifier[K <: Symbol, V <: Option[Modifier[_]]](
        implicit wt: Witness.Aux[K]
    ): Case.Aux[FieldType[K, V], FieldType[K, (String, Option[Fragment])]] =
      at[FieldType[K, V]] { ft =>
        val name = wt.value.name
        field[K](ft.map(f => (name, Some(f.toFragment))).getOrElse((name, None)))
      }
  }

  object relationFilterToOptionFragment extends Poly1 {
    implicit def atOptionRelationFilter[
        A,
        B,
        FB <: EntityFilter[FB],
        K <: Symbol,
        V <: Option[RelationFilter[A, B, FB]]
    ](
        implicit
        wt: Witness.Aux[K],
        link: TableLink[A, B],
        tableFilter: TableFilter[B, FB]
    ): Case.Aux[FieldType[K, V], FieldType[K, Option[String] => Option[Fragment]]] =
      at[FieldType[K, V]] { ft =>
        field[K]((alias: Option[String]) =>
          ft.flatMap {
            if (link.isJunction) {
              processJunctionRelation(
                alias,
                link.leftSyntax,
                link.rightSyntax,
                link.junctionSyntax,
                link.leftJoinFields,
                link.rightJoinFields,
                tableFilter
              )
            } else {
              processDirectRelation(
                alias,
                link.leftSyntax,
                link.rightSyntax,
                link.leftJoinFields,
                tableFilter
              )
            }
          }
        )
      }

    implicit def atOptionRelationFilterInverse[A, B, FB <: EntityFilter[FB], K <: Symbol, V <: Option[
      RelationFilter[A, B, FB]
    ]](
        implicit
        wt: Witness.Aux[K],
        link: TableLink[B, A],
        tableFilter: TableFilter[B, FB]
    ): Case.Aux[FieldType[K, V], FieldType[K, Option[String] => Option[Fragment]]] =
      atOptionRelationFilter(wt, link.inverse, tableFilter)
  }

  private def processDirectRelation[A, B, FB <: EntityFilter[FB]](
      alias: Option[String],
      leftSyntax: TableSyntax[A],
      rightSyntax: TableSyntax[B],
      joinFields: List[(String, String)],
      tableFilter: TableFilter[B, FB]
  ): RelationFilter[A, B, FB] => Option[Fragment] = f => {
    val leftAlias       = alias.getOrElse("")
    val leftQuerySyntax = leftSyntax.withAlias(leftAlias)
    val rightAlias = if (leftQuerySyntax.alias == rightSyntax.alias) { // name clash
      s"${rightSyntax.alias}_self"
    } else {
      rightSyntax.alias
    }
    val rightQuerySyntax = rightSyntax.withAlias(rightAlias)
    val joinCondition = joinFields
      .map {
        case (lf, rf) => s"${leftQuerySyntax.aliasedColumn(lf)} = ${rightQuerySyntax.aliasedColumn(rf)}"
      }
      .mkString(s" $And ")
    // DIRECT LINK - EVERY
    // EXISTS(
    //  SELECT ONE WHERE
    //  (SELECT COUNT(*) FROM (SELECT * FROM rightTable WHERE filter)
    //    as rightTable WHERE rightTable.id = leftTable.id)
    //   =
    //   (SELECT COUNT(*) FROM rightTable WHERE rightTable.id = leftTable.id)
    // )
    val makeEveryFragment = (filterFragment: Fragment) =>
      const(
        s"$Exists (" +
          s"$Select 1 $Where " +
          s"($Select $Count ($Star) $From ${rightQuerySyntax.aliasedName} $Where $joinCondition $And"
      ) ++ filterFragment ++
        const(s") = ($Select $Count ($Star) $From ${rightQuerySyntax.aliasedName} $Where $joinCondition))")
    val every = f.EVERY
      .flatMap(tableFilter.byFilterFragment(_, StringUtils.strToOpt(rightAlias)))
      .map(makeEveryFragment)
    // DIRECT LINK - SOME (contrary of NONE)
    // EXISTS(
    //   SELECT ONE
    //   FROM (SELECT * FROM rightTable WHERE filter) as rightTable
    //   WHERE rightTable.id = leftTable.id
    // )
    val makeSomeFragment = (filterFragment: Fragment) =>
      const(s"$Exists ($Select 1 $From ${rightQuerySyntax.aliasedName} $Where $joinCondition $And ") ++
        filterFragment ++ const(")")
    val some = f.SOME
      .flatMap(tableFilter.byFilterFragment(_, StringUtils.strToOpt(rightAlias)))
      .map(makeSomeFragment)
    // DIRECT LINK -  NONE (contrary of SOME)
    // NOT EXISTS(
    //   SELECT ONE
    //   FROM (SELECT * FROM rightTable WHERE filter) as rightTable
    //   WHERE rightTable.id = leftTable.id
    // )
    val makeNoneFragment = (filterFragment: Fragment) =>
      const(s"$NotExists ($Select 1 $From ${rightQuerySyntax.aliasedName} $Where $joinCondition $And ") ++
        filterFragment ++ const(")")
    val none = f.NONE
      .flatMap(tableFilter.byFilterFragment(_, StringUtils.strToOpt(rightAlias)))
      .map(makeNoneFragment)
    // AND between EVERY, SOME, NONE
    FragmentUtils.optionalAndOpt(every, some, none)
  }

  private def processJunctionRelation[A, B, C, FB <: EntityFilter[FB]](
      alias: Option[String],
      leftSyntax: TableSyntax[A],
      rightSyntax: TableSyntax[B],
      junctionSyntax: TableSyntax[C],
      leftJoinFields: List[(String, String)],
      rightJoinFields: List[(String, String)],
      tableFilter: TableFilter[B, FB]
  ): RelationFilter[A, B, FB] => Option[Fragment] = f => {
    val leftQuerySyntax   = leftSyntax.withAlias(alias.getOrElse(""))
    val rightQuerySyntax  = rightSyntax // TODO: add right alias to handle self joins
    val middleQuerySyntax = junctionSyntax
    val leftCond          = leftJoinFields
    val rightCond         = rightJoinFields
    val leftCondSql = leftCond
      .map {
        case (lf, rf) => s"${leftQuerySyntax.column(lf)} = ${middleQuerySyntax.column(rf)}"
      }
      .mkString(s" $And ")
    val rightCondSql = rightCond
      .map {
        case (lf, rf) => s"${rightQuerySyntax.column(lf)} = ${middleQuerySyntax.column(rf)}"
      }
      .mkString(s" $And ")
    // JUNCTION LINK - EVERY
    // NOT EXISTS(
    //   SELECT ONE
    //   FROM joinTable
    //   LEFT JOIN (SELECT * FROM rightTable WHERE filter) AS rightTable
    //   ON joinTable.id = rightTable.id
    //   WHERE joinTable.id = leftTable.id AND IS NULL rightTable.id
    // )
    val rightIsNull = rightCond.map(_._1).map(r => s"$IsNull $r").mkString(s" $And ")
    val makeEveryFragment = (filterFragment: Fragment) =>
      const(s"""
                        |$NotExists (
                        |$Select 1
                        |$From ${middleQuerySyntax.aliasedName} $LeftJoin ${rightQuerySyntax.aliasedName}
                        |$On ${rightCondSql}
                        |$Where ${leftCondSql} $And $rightIsNull $And
                        |""".stripMargin) ++ filterFragment ++
        const(")")
    val every = f.EVERY
      .flatMap(tableFilter.byFilterFragment(_, None))
      .map(makeEveryFragment)
    // JUNCTION LINK - SOME (contrary of NONE)
    // (YOU CAN USE LEFT JOIN AND ADD "IS NOT NULL rightTable.id")
    // EXISTS(
    //   SELECT ONE
    //   FROM joinTable
    //   INNER JOIN (SELECT * FROM rightTable WHERE filter) AS rightTable
    //   ON joinTable.id = rightTable.id
    //   WHERE joinTable.id = leftTable.id
    // )
    val makeSomeFragment = (filterFragment: Fragment) =>
      const(s"""
                        |$Exists (
                        |$Select 1
                        |$From ${middleQuerySyntax.aliasedName} $InnerJoin ${rightQuerySyntax.aliasedName}
                        |$On ${rightCondSql}
                        |$Where ${leftCondSql} $And
                        |""".stripMargin) ++ filterFragment ++
        const(")")
    val some = f.SOME
      .flatMap(tableFilter.byFilterFragment(_, None))
      .map(makeSomeFragment)
    // JUNCTION LINK - NONE (contrary of SOME)
    // (YOU CAN USE LEFT JOIN AND ADD "IS NOT NULL rightTable.id")
    // NOT EXISTS(
    //   SELECT ONE
    //   FROM joinTable
    //   INNER JOIN (SELECT * FROM rightTable WHERE filter) AS rightTable
    //   ON joinTable.id = rightTable.id
    //   WHERE joinTable.id = leftTable.id
    // )
    val makeNoneFragment = (filterFragment: Fragment) =>
      const(s"""
                        |$NotExists (
                        |$Select 1
                        |$From ${middleQuerySyntax.aliasedName} $InnerJoin ${rightQuerySyntax.aliasedName}
                        |$On ${rightCondSql}
                        |$Where ${leftCondSql} $And
                        |""".stripMargin) ++ filterFragment ++
        const(")")
    val none = f.NONE
      .flatMap(tableFilter.byFilterFragment(_, None))
      .map(makeNoneFragment)
    // AND between EVERY, SOME, NONE
    FragmentUtils.optionalAndOpt(every, some, none)
  }
}
