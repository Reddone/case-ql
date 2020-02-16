package com.github.reddone.caseql.sql

import cats.data.NonEmptyList
import com.github.reddone.caseql.sql.TestModel._
import com.github.reddone.caseql.sql.filter.models.{IntFilter, LongFilter, StringFilter}
import com.github.reddone.caseql.sql.filter.wrappers.{EntityFilter, RelationFilter}
import com.github.reddone.caseql.sql.table.TableFunction.{extractRelationFilter, relationFilterToOptionFragment}
import com.github.reddone.caseql.sql.table.TableLink.Aux
import com.github.reddone.caseql.sql.table.{ColSet, Table, TableFilter, TableLink, TableModifier}
import doobie._
import doobie.implicits._
import javasql._
import javatime._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import shapeless.LabelledGeneric

class SpecPlayground extends AnyFlatSpec with Matchers {

  implicit val table: Table[Test, TestKey]                      = Table.derive[Test, TestKey]()
  implicit val tableFilter: TableFilter[Test, TestFilter]       = TableFilter.derive[Test, TestFilter]()
  implicit val tableModifier: TableModifier[Test, TestModifier] = TableModifier.derive[Test, TestModifier]()

  case class A(field1: String, field2: Int)
  case class AKey(field1: String)
  case class B(field1: Long, field2: String)
  case class BKey(field1: Long)
  case class AFilter(
      field1: Option[StringFilter],
      field2: Option[IntFilter],
      bRelation: Option[RelationFilter[A, B, BFilter]]
  ) extends EntityFilter[AFilter] {
    override def AND: Option[Seq[AFilter]] = None
    override def OR: Option[Seq[AFilter]]  = None
    override def NOT: Option[AFilter]      = None
  }

  case class BFilter(
      field1: Option[LongFilter],
      field2: Option[StringFilter]
  ) extends EntityFilter[BFilter] {
    override def AND: Option[Seq[BFilter]] = None
    override def OR: Option[Seq[BFilter]]  = None
    override def NOT: Option[BFilter]      = None
  }

  "SpecPlayground" should "do anything" in {
    implicit val tableA: Table[A, AKey] = Table.derive[A, AKey]()
    implicit val tableB: Table[B, BKey] = Table.derive[B, BKey]()
    implicit val relAB: TableLink[A, B] = TableLink.direct(tableA, tableB) { (a, b) =>
      NonEmptyList.of(("field1", "field2"))
    }
    implicit val filterB: TableFilter[B, BFilter] = TableFilter.derive[B, BFilter]()
    implicit val filterA: TableFilter[A, AFilter] = TableFilter.derive[A, AFilter]()

    val bFilter = BFilter(
      Some(LongFilter.empty.copy(EQ = Some(1L))),
      Some(StringFilter.empty)
    )
    val aFilter = AFilter(
      None,
      None,
      Some(RelationFilter[A, B, BFilter](Some(bFilter), None, None))
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

  it should "process links" in {
    implicit val tableA: Table[A, AKey] = Table.derive[A, AKey]()
    implicit val tableB: Table[B, BKey] = Table.derive[B, BKey]()
    implicit val linkAB: Aux[A, B, Unit] =
      TableLink.safe[A, B].apply(ColSet("field1"), ColSet("field2"))

    println(linkAB.leftJoinFields)
  }
}
