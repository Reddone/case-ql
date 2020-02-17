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

  implicit val leftRightLink: Aux[TestLeft, TestRight, Unit] =
    TableLink.direct[TestLeft, TestRight](FieldSet("field1"), FieldSet("field3"))

  //val b: TableLink[TestRight, TestLeft] = implicitly[TableLink[TestRight, TestLeft]]

  "TableLink implicit resolution" should "compile" in {
    """implicitly[TableLink[TestRight, TestLeft]]""" should compile
  }

  "TableLink methods" should "work correctly" in {}
}
