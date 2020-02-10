package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.filter.models.Filter
import com.github.reddone.caseql.sql.filter.wrappers.{EntityFilter, RelationFilter}
import com.github.reddone.caseql.sql.modifier.models.Modifier
import com.github.reddone.caseql.sql.tokens._
import com.github.reddone.caseql.sql.util.FragmentUtils
import doobie._
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
    implicit def atOptionRelationFilter[A, B, FB <: EntityFilter[FB], K <: Symbol, V <: Option[
      RelationFilter[A, B, FB]
    ]](
        implicit
        wt: Witness.Aux[K],
        link: TableLink[A, B],
        tableFilter: TableFilter[B, FB]
    ): Case.Aux[FieldType[K, V], FieldType[K, Option[String] => Option[Fragment]]] =
      at[FieldType[K, V]] { ft =>
        field[K]((alias: Option[String]) =>
          ft.flatMap(f =>
            if (link.isJunction) {
              val leftQuerySyntax   = link.leftSyntax.withAlias(alias)
              val rightQuerySyntax  = link.rightSyntax // TODO: add right alias to handle self joins
              val middleQuerySyntax = link.junctionSyntax
              val leftCond          = link.leftJoinFields
              val rightCond         = link.rightJoinFields
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
              val sqlStringEvery = (filterFrag: Fragment) =>
                Fragment.const(s"""
                                  |$NotExists (
                                  |$Select 1
                                  |$From ${middleQuerySyntax.aliasedName} $LeftJoin ${rightQuerySyntax.aliasedName}
                                  |$On ${rightCondSql}
                                  |$Where ${leftCondSql} $And $rightIsNull $And
                                  |""".stripMargin) ++ filterFrag ++
                  Fragment.const(")")
              val every = f.EVERY.flatMap(ff => rightQuerySyntax.support.byFilterFragment(ff, None)).map(sqlStringEvery)
              // JUNCTION LINK - SOME (contrary of NONE)
              // (YOU CAN USE LEFT JOIN AND ADD "IS NOT NULL rightTable.id")
              // EXISTS(
              //   SELECT ONE
              //   FROM joinTable
              //   INNER JOIN (SELECT * FROM rightTable WHERE filter) AS rightTable
              //   ON joinTable.id = rightTable.id
              //   WHERE joinTable.id = leftTable.id
              // )
              val sqlStringSome = (filterFrag: Fragment) =>
                Fragment.const(s"""
                                  |$Exists (
                                  |$Select 1
                                  |$From ${middleQuerySyntax.aliasedName} $InnerJoin ${rightQuerySyntax.aliasedName}
                                  |$On ${rightCondSql}
                                  |$Where ${leftCondSql} $And
                                  |""".stripMargin) ++ filterFrag ++
                  Fragment.const(")")
              val some = f.SOME.flatMap(ff => rightQuerySyntax.support.byFilterFragment(ff, None)).map(sqlStringSome)
              // JUNCTION LINK - NONE (contrary of SOME)
              // (YOU CAN USE LEFT JOIN AND ADD "IS NOT NULL rightTable.id")
              // NOT EXISTS(
              //   SELECT ONE
              //   FROM joinTable
              //   INNER JOIN (SELECT * FROM rightTable WHERE filter) AS rightTable
              //   ON joinTable.id = rightTable.id
              //   WHERE joinTable.id = leftTable.id
              // )
              val sqlStringNone = (filterFrag: Fragment) =>
                Fragment.const(s"""
                                  |$NotExists (
                                  |$Select 1
                                  |$From ${middleQuerySyntax.aliasedName} $InnerJoin ${rightQuerySyntax.aliasedName}
                                  |$On ${rightCondSql}
                                  |$Where ${leftCondSql} $And
                                  |""".stripMargin) ++ filterFrag ++
                  Fragment.const(")")
              val none = f.NONE.flatMap(ff => rightQuerySyntax.support.byFilterFragment(ff, None)).map(sqlStringNone)
              // combine everything together
              FragmentUtils.optionalAndOpt(every, some, none)
            } else {
              val leftQuerySyntax  = link.leftSyntax.withAlias(alias)
              val rightQuerySyntax = link.rightSyntax // TODO: add right alias to handle self joins
              val leftCond         = link.leftJoinFields
              val leftCondSql = leftCond
                .map {
                  case (lf, rf) => s"${leftQuerySyntax.column(lf)} = ${rightQuerySyntax.column(rf)}"
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
              val sqlStringEvery = (filterFrag: Fragment) =>
                Fragment.const(s"""
                                  |$Exists (
                                  |$Select 1 $Where
                                  |(
                                  |$Select $Count ($Star) $From ${rightQuerySyntax.aliasedName} $Where $leftCondSql $And
                                  """.stripMargin) ++ filterFrag ++
                  Fragment.const(s"""
                                  |) = (
                                  |$Select $Count ($Star) $From ${rightQuerySyntax.aliasedName} $Where $leftCondSql)
                                  |)
                                  |""".stripMargin)
              val every = f.EVERY.flatMap(ff => rightQuerySyntax.support.byFilterFragment(ff, None)).map(sqlStringEvery)
              // DIRECT LINK - SOME (contrary of NONE)
              // EXISTS(
              //   SELECT ONE
              //   FROM (SELECT * FROM rightTable WHERE filter) as rightTable
              //   WHERE rightTable.id = leftTable.id
              // )
              val sqlStringSome = (filterFrag: Fragment) =>
                Fragment.const(s"""
                                  |$Exists (
                                  |$Select 1
                                  |$From ${rightQuerySyntax.aliasedName}
                                  |$Where ${leftCondSql} $And
                                  |""".stripMargin) ++ filterFrag ++
                  Fragment.const(")")
              val some = f.SOME.flatMap(ff => rightQuerySyntax.support.byFilterFragment(ff, None)).map(sqlStringSome)
              // DIRECT LINK -  NONE (contrary of SOME)
              // NOT EXISTS(
              //   SELECT ONE
              //   FROM (SELECT * FROM rightTable WHERE filter) as rightTable
              //   WHERE rightTable.id = leftTable.id
              // )
              val sqlStringNone = (filterFrag: Fragment) =>
                Fragment.const(s"""
                                  |$NotExists (
                                  |$Select 1
                                  |$From ${rightQuerySyntax.aliasedName}
                                  |$Where ${leftCondSql} $And
                                  |""".stripMargin) ++ filterFrag ++
                  Fragment.const(")")
              val none = f.NONE.flatMap(ff => rightQuerySyntax.support.byFilterFragment(ff, None)).map(sqlStringNone)
              // combine everything together
              FragmentUtils.optionalAndOpt(every, some, none)
            }
          )
        )
      }
  }
}
