package com.github.reddone.caseql.sql.generic

import com.github.reddone.caseql.sql.filter.models.Filter
import com.github.reddone.caseql.sql.filter.wrappers.{EntityFilter, RelationFilter}
import com.github.reddone.caseql.sql.modifier.models.Modifier
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
    implicit def atOptionRelationFilter[A, B, FB <: EntityFilter[B, FB], K <: Symbol, V <: Option[
      RelationFilter[A, B, FB]
    ]](
        implicit
        wt: Witness.Aux[K],
        link: Link[A, B],
        tableFilter: TableFilter[B, FB]
    ): Case.Aux[FieldType[K, V], FieldType[K, Option[Fragment]]] =
      at[FieldType[K, V]] { ft =>
        field[K](
          ft.flatMap(f =>
            // use TableFilterFB to derive a where condition for right inside a subquery
            if (link.isJunction) {
              // Single relation is implemented using a junction table
              val left      = link.tableA.syntax("l")
              val right     = link.tableB.syntax("r")
              val middle    = link.tableC.syntax("m")
              val leftCond  = link.leftJoinFields
              val rightCond = link.rightJoinFields
              // EVERY
              // NOT EXISTS(
              //   SELECT ONE
              //   FROM joinTable
              //   LEFT JOIN (SELECT * FROM rightTable WHERE filter) AS rightTable
              //   ON jointable.id = rightTable.id
              //   WHERE joinTable.id = leftTable.id AND IS NULL rightTable.id
              // )

              val a = (a: Table[A]#Syntax) => 3

              a(link.tableA.defaultSyntax)

              val leftCondSql = leftCond
                .map {
                  case (lf, rf) => s"${left.column(lf)} = ${middle.column(rf)}"
                }
                .mkString(" AND ")
              val rightCondSql = rightCond
                .map {
                  case (lf, rf) => s"${right.column(lf)} = ${middle.column(rf)}"
                }
                .mkString(" AND ")
              val rightIsNull = rightCond.map(_._1).map(r => s"IS NULL $r").mkString(" AND ")
              val sqlString = (filterFrag: Fragment) =>
                Fragment.const(s"""
                                  |NOT EXISTS (
                                  |SELECT ONE
                                  |FROM ${middle.name} LEFT JOIN ${right.name}
                                  |ON ${rightCondSql}
                                  |WHERE ${leftCondSql} AND $rightIsNull AND
                                  |""".stripMargin) ++ filterFrag ++
                  Fragment.const(")")

              //f.EVERY.flatMap(ff => EntityFilter2.filterFragment[B, T](ff, right)(tableFilter)).map(sqlString)
              // SOME
              // (YOU CAN USE LEFT JOIN AND ADD "IS NOT NULL rightTable.id") - Contrary of NONE
              // EXISTS(
              //   SELECT ONE
              //   FROM joinTable
              //   INNER JOIN (SELECT * FROM rightTable WHERE filter) AS rightTable
              //   ON jointable.id = rightTable.id
              //   WHERE joinTable.id = leftTable.id
              // )

              // NONE
              // (YOU CAN USE LEFT JOIN AND ADD "IS NOT NULL rightTable.id") - Contrary of SOME
              // NOT EXISTS(
              //   SELECT ONE
              //   FROM joinTable
              //   INNER JOIN (SELECT * FROM rightTable WHERE filter) AS rightTable
              //   ON jointable.id = rightTable.id
              //   WHERE joinTable.id = leftTable.id
              // )

              None
            } else {
              // we have to always re-alias the right table because of self joins
              // We can use the same syntax inside filters by wrapping everything inside a sub query

              // Single relation is implemented using a direct table
              val left     = link.tableA.defaultSyntax //.syntax("l")
              val right    = link.tableB.defaultSyntax //.syntax("r")
              val leftCond = link.leftJoinFields
              val leftCondSql = leftCond
                .map {
                  case (lf, rf) => s"${left.column(lf)} = ${right.column(rf)}"
                }
                .mkString(" AND ")
              // EVERY
              // EXISTS(
              //  SELECT ONE WHERE
              //  (SELECT COUNT(*) FROM (SELECT * FROM rightTable WHERE filter)
              //    as rightTable WHERE rightTable.id = leftTable.id)
              //   =
              //   (SELECT COUNT(*) FROM rightTable WHERE rightTable.id = leftTable.id
              // )

              // SOME
              // EXISTS(
              //   SELECT ONE
              //   FROM (SELECT * FROM rightTable WHERE filter) as rightTable
              //   WHERE rightTable.id = leftTable.id
              // )
              val sqlString = (filterFrag: Fragment) =>
                Fragment.const(s"""
                                  |EXISTS (
                                  |SELECT ONE
                                  |FROM ${right.name}
                                  |WHERE ${leftCondSql} AND
                                  |""".stripMargin) ++ filterFrag ++
                  Fragment.const(")")

              //f.EVERY.flatMap(ff => EntityFilter2.filterFragment[B, T](ff, right)(tableFilter)).map(sqlString)
              // NONE
              // NOT EXISTS(
              //   SELECT ONE
              //   FROM (SELECT * FROM rightTable WHERE filter) as rightTable
              //   WHERE rightTable.id = leftTable.id
              // )

              None
            }
          )
        )
      }
  }
}
