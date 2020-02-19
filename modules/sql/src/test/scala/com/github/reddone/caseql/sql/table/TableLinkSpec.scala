package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.table.TableLink.Aux
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TableLinkSpec extends AnyFlatSpec with Matchers {

  case class TestLeft(
      field1: Int,
      field2: String,
      field3: Int
  )
  case class TestLeftKey(
      field1: Int
  )

  case class TestRight(
      field1: Long,
      field2: String,
      field3: Int
  )
  case class TestRightKey(
      field1: Long
  )

  case class TestJunction(
      field1: Int,
      field2: Long
  )
  case class TestJunctionKey(
      field1: Int,
      field2: Long
  )

  implicit val leftTable: Table[TestLeft, TestLeftKey]             = Table.derive[TestLeft, TestLeftKey]()
  implicit val rightTable: Table[TestRight, TestRightKey]          = Table.derive[TestRight, TestRightKey]()
  implicit val junctionTable: Table[TestJunction, TestJunctionKey] = Table.derive[TestJunction, TestJunctionKey]()

  "TableLink derivation" should "compile for a correct self link" in {
    """TableLink.self[TestLeft](
          |FieldSet("field1"), 
          |FieldSet("field3")
          |)""".stripMargin should compile
    """TableLink.self[TestLeft](
          |FieldSet("field3", "field1"), 
          |FieldSet("field1", "field3")
          |)""".stripMargin should compile
  }

  it should "not compile for a wrong self link" in {
    """TableLink.self[TestLeft](
          |FieldSet("field1"), 
          |FieldSet("field2")
          |)""".stripMargin shouldNot compile
    """TableLink.self[TestLeft](
          |FieldSet("field1", "field3"), 
          |FieldSet("field2", "field3")
          |)""".stripMargin shouldNot compile
  }

  it should "compile for a correct direct link" in {
    """TableLink.direct[TestLeft, TestJunction](
          |FieldSet("field1"),
          |FieldSet("field1")
          |)""".stripMargin should compile
    """TableLink.direct[TestLeft, TestRight](
          |FieldSet("field1"),
          |FieldSet("field3")
          |)""".stripMargin should compile
  }

  it should "not compile for a wrong direct link" in {
    """TableLink.direct[TestLeft, TestJunction](
      |FieldSet("field1"),
      |FieldSet("field2")
      |)""".stripMargin shouldNot compile
    """TableLink.direct[TestLeft, TestRight](
      |FieldSet("field1"),
      |FieldSet("field2")
      |)""".stripMargin shouldNot compile
  }

  "TableLink implicit resolution" should "compile for an inverse link" in {
    implicit val leftJunctionLink: Aux[TestLeft, TestJunction, Unit] =
      TableLink.direct[TestLeft, TestJunction](FieldSet("field1"), FieldSet("field1"))
    implicit val leftRightLink: Aux[TestLeft, TestRight, Unit] =
      TableLink.direct[TestLeft, TestRight](FieldSet("field1"), FieldSet("field3"))
    """implicitly[TableLink[TestJunction, TestLeft]]""" should compile
    """implicitly[TableLink[TestRight, TestLeft]]""" should compile
  }

  it should "compile for a junction link" in {
    implicit val leftJunctionLink: Aux[TestLeft, TestJunction, Unit] =
      TableLink.direct[TestLeft, TestJunction](FieldSet("field1"), FieldSet("field1"))
    implicit val rightJunctionLink: Aux[TestRight, TestJunction, Unit] =
      TableLink.direct[TestRight, TestJunction](FieldSet("field1"), FieldSet("field2"))
    """implicitly[TableLink[TestLeft, TestRight]]""" should compile
  }

  it should "compile for a junction link made of inverse links" in {
    implicit val junctionLeftLink: Aux[TestJunction, TestLeft, Unit] =
      TableLink.direct[TestJunction, TestLeft](FieldSet("field1"), FieldSet("field1"))
    implicit val junctionRightLink: Aux[TestJunction, TestRight, Unit] =
      TableLink.direct[TestJunction, TestRight](FieldSet("field2"), FieldSet("field1"))
    """implicitly[TableLink[TestLeft, TestRight]]""" should compile
  }

  "TableLink typeclass" should "work correctly on a self link" in {
    val link: Aux[TestLeft, TestLeft, Unit] = TableLink.self[TestLeft](
      FieldSet("field3", "field1"),
      FieldSet("field1", "field3")
    )

    link.leftSyntax shouldBe leftTable.syntax
    link.rightSyntax shouldBe leftTable.syntax
    link.junctionSyntax shouldBe Table.unit.syntax
  }

  it should "work correctly on a direct link" in {
    val link: Aux[TestLeft, TestRight, Unit] = TableLink.direct[TestLeft, TestRight](
      FieldSet("field1"),
      FieldSet("field3")
    )

    link.leftSyntax shouldBe leftTable.syntax
    link.rightSyntax shouldBe rightTable.syntax
    link.junctionSyntax shouldBe Table.unit.syntax
  }

  it should "work correctly on an inverse link" in {
    implicit val leftRightLink: Aux[TestLeft, TestRight, Unit] = TableLink.direct[TestLeft, TestRight](
      FieldSet("field1"),
      FieldSet("field3")
    )
    val link = implicitly[TableLink[TestRight, TestLeft]]
  }

  it should "work correctly on a junction link" in {
    implicit val leftJunctionLink: Aux[TestLeft, TestJunction, Unit] =
      TableLink.direct[TestLeft, TestJunction](FieldSet("field1"), FieldSet("field1"))
    implicit val rightJunctionLink: Aux[TestRight, TestJunction, Unit] =
      TableLink.direct[TestRight, TestJunction](FieldSet("field1"), FieldSet("field2"))
    val link = implicitly[TableLink[TestLeft, TestRight]]
  }
}
