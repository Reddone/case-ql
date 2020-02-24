package com.github.reddone.caseql.sql

import com.github.reddone.caseql.sql.TestModel._
import com.github.reddone.caseql.sql.filter.models.{IntFilter, LongFilter, StringFilter}
import com.github.reddone.caseql.sql.filter.wrappers.{EntityFilter, RelationFilter}
import com.github.reddone.caseql.sql.table.TableFunction.{extractRelationFilter, relationFilterToOptionFragment}
import com.github.reddone.caseql.sql.table.TableLink.Aux
import com.github.reddone.caseql.sql.table.{FieldSet, Table, TableFilter, TableLink, TableModifier}
import doobie._
import doobie.implicits._
import javasql._
import javatime._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import shapeless.LabelledGeneric

class SpecPlayground extends AnyFlatSpec with Matchers {

  case class A(field1: String, field2: Int)
  case class AKey(field1: String)

  case class B(field1: Long, field2: String)
  case class BKey(field1: Long)

  case class AFilter(
      field1: Option[StringFilter],
      field2: Option[IntFilter],
      bRelation: Option[RelationFilter[A, B, BFilter]],
      AND: Option[Seq[AFilter]],
      OR: Option[Seq[AFilter]],
      NOT: Option[AFilter]
  ) extends EntityFilter[AFilter]

  case class BFilter(
      field1: Option[LongFilter],
      field2: Option[StringFilter],
      AND: Option[Seq[BFilter]],
      OR: Option[Seq[BFilter]],
      NOT: Option[BFilter]
  ) extends EntityFilter[BFilter]

  "SpecPlayground" should "do anything" in {
    implicit val tableA: Table[A, AKey]  = Table.derive[A, AKey]()
    implicit val tableB: Table[B, BKey]  = Table.derive[B, BKey]()
    implicit val linkAB: Aux[A, B, Unit] = TableLink.direct[A, B](FieldSet("field1"), FieldSet("field2"))

    implicit val filterB: TableFilter[B, BFilter] = TableFilter.derive[B, BFilter]()
    implicit val filterA: TableFilter[A, AFilter] = TableFilter.derive[A, AFilter]()

    val bFilter = BFilter(
      Some(LongFilter.empty.copy(EQ = Some(1L))),
      Some(StringFilter.empty),
      None,
      None,
      None
    )
    val aFilter = AFilter(
      None,
      None,
      Some(RelationFilter[A, B, BFilter](Some(bFilter), None, None)),
      None,
      None,
      None
    )

    println(
      LabelledGeneric[AFilter]
        .to(aFilter)
        .flatMap(extractRelationFilter)
        .map(relationFilterToOptionFragment)
        .toList[Option[String] => Option[Fragment]]
        .map(_.apply(None))
    )
  }

  it should "resolve relation implicits" in {
    // tables
    implicit val leftTable: Table[TestLeft, TestLeftKey]             = Table.derive[TestLeft, TestLeftKey]()
    implicit val directTable: Table[TestDirect, TestDirectKey]       = Table.derive[TestDirect, TestDirectKey]()
    implicit val rightTable: Table[TestRight, TestRightKey]          = Table.derive[TestRight, TestRightKey]()
    implicit val junctionTable: Table[TestJunction, TestJunctionKey] = Table.derive[TestJunction, TestJunctionKey]()
    // links
    implicit val leftSelfLink: Aux[TestLeft, TestLeft, Unit] = TableLink.self[TestLeft](
      FieldSet("field1"),
      FieldSet("field3")
    )
    implicit val directLeftLink: Aux[TestDirect, TestLeft, Unit] = TableLink.direct[TestDirect, TestLeft](
      FieldSet("field3"),
      FieldSet("field1")
    )
    implicit val leftJunctionLink: Aux[TestLeft, TestJunction, Unit] = TableLink.direct[TestLeft, TestJunction](
      FieldSet("field1"),
      FieldSet("field1")
    )
    implicit val rightJunctionLink: Aux[TestRight, TestJunction, Unit] = TableLink.direct[TestRight, TestJunction](
      FieldSet("field1"),
      FieldSet("field2")
    )
    implicit val leftRightLink: Aux[TestLeft, TestRight, TestJunction] =
      TableLink.union(leftJunctionLink, rightJunctionLink)
    // table filters
    implicit val rightTableFilter: TableFilter[TestRight, TestRightFilter] =
      TableFilter.derive[TestRight, TestRightFilter]()
    implicit val leftTableFilter: TableFilter[TestLeft, TestLeftFilter] =
      TableFilter.derive[TestLeft, TestLeftFilter]()


  }
}
