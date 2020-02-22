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
          |FieldSet("field3")
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

  it should "compile for a correct junction link" in {
    """TableLink.junction[TestLeft, TestRight, TestJunction](
      |(FieldSet("field1"), FieldSet("field1")),
      |(FieldSet("field1"), FieldSet("field2"))
      |)""".stripMargin should compile
  }

  it should "not compile for a wrong junction link" in {
    """TableLink.junction[TestLeft, TestRight, TestJunction](
      |(FieldSet("field1"), FieldSet("field2")),
      |(FieldSet("field1"), FieldSet("field2"))
      |)""".stripMargin shouldNot compile
  }

  "TableLink typeclass" should "work correctly on a self link" in {
    val link: Aux[TestLeft, TestLeft, Unit] = TableLink.self[TestLeft](
      FieldSet("field3", "field1"),
      FieldSet("field1", "field3")
    )

    link.leftSyntax shouldBe leftTable.syntax
    link.rightSyntax shouldBe leftTable.syntax
    link.junctionSyntax shouldBe Table.unit.syntax
    link.leftJoinFields shouldBe List(("field3", "field1"), ("field1", "field3"))
    link.rightJoinFields shouldBe List(("field1", "field3"), ("field3", "field1"))
    link.isJunction shouldBe false
  }

  it should "work correctly on a direct link" in {
    val link: Aux[TestLeft, TestRight, Unit] = TableLink.direct[TestLeft, TestRight](
      FieldSet("field1"),
      FieldSet("field3")
    )

    link.leftSyntax shouldBe leftTable.syntax
    link.rightSyntax shouldBe rightTable.syntax
    link.junctionSyntax shouldBe Table.unit.syntax
    link.leftJoinFields shouldBe List(("field1", "field3"))
    link.rightJoinFields shouldBe List(("field3", "field1"))
    link.isJunction shouldBe false
  }

  it should "work correctly on an inverse link" in {
    val leftRightLink: Aux[TestLeft, TestRight, Unit] = TableLink.direct[TestLeft, TestRight](
      FieldSet("field1"),
      FieldSet("field3")
    )
    val link: Aux[TestRight, TestLeft, Unit] = leftRightLink.inverse

    link.leftSyntax shouldBe rightTable.syntax
    link.rightSyntax shouldBe leftTable.syntax
    link.junctionSyntax shouldBe Table.unit.syntax
    link.leftJoinFields shouldBe List(("field3", "field1"))
    link.rightJoinFields shouldBe List(("field1", "field3"))
    link.isJunction shouldBe false
  }

  it should "work correctly on a junction link" in {
    val leftJunctionLink: Aux[TestLeft, TestJunction, Unit] =
      TableLink.direct[TestLeft, TestJunction](FieldSet("field1"), FieldSet("field1"))
    val rightJunctionLink: Aux[TestRight, TestJunction, Unit] =
      TableLink.direct[TestRight, TestJunction](FieldSet("field1"), FieldSet("field2"))
    val link1: Aux[TestLeft, TestRight, TestJunction] =
      TableLink.union(leftJunctionLink, rightJunctionLink)

    link1.leftSyntax shouldBe leftTable.syntax
    link1.rightSyntax shouldBe rightTable.syntax
    link1.junctionSyntax shouldBe junctionTable.syntax
    link1.leftJoinFields shouldBe List(("field1", "field1"))
    link1.rightJoinFields shouldBe List(("field1", "field2"))
    link1.isJunction shouldBe true

    val link2 = TableLink.junction[TestLeft, TestRight, TestJunction](
      (FieldSet("field1"), FieldSet("field1")),
      (FieldSet("field1"), FieldSet("field2"))
    )

    link2.leftSyntax shouldBe leftTable.syntax
    link2.rightSyntax shouldBe rightTable.syntax
    link2.junctionSyntax shouldBe junctionTable.syntax
    link2.leftJoinFields shouldBe List(("field1", "field1"))
    link2.rightJoinFields shouldBe List(("field1", "field2"))
    link2.isJunction shouldBe true
  }
}
