package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.TestModel._
import doobie._
import doobie.implicits._
import javasql._
import javatime._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import shapeless.test.illTyped

class TableSpec extends AnyFlatSpec with Matchers {

  "Table derivation" should "compile in the simple case" in {
    """Table.derive[Test, TestKey]()""" should compile
  }

  it should "compile in the unordered case" in {
    """Table.derive[Test, TestKeyUnordered]()""" should compile
  }

  it should "not compile in the other case" in {
    """Table.derive[Test, TestKeyOther]()""" shouldNot compile
    illTyped { """Table.derive[Test, TestKeyOther]()""" }
  }

  it should "not compile in the other unordered case" in {
    """Table.derive[Test, TestKeyOtherUnordered]()""" shouldNot compile
    illTyped { """Table.derive[Test, TestKeyOtherUnordered]()""" }
  }

  "Table typeclass" should "work correctly" in {
    val table1: Table[Test, TestKey] = Table.derive[Test, TestKey]()

    table1.name shouldBe "test"
    table1.schema shouldBe None
    table1.fieldConverter.isEmpty shouldBe true
    table1.fieldMapper("CamelCase") shouldBe "camel_case"
    table1.fields shouldBe List("field1", "field2", "field3", "field4")
    table1.keyFields shouldBe List("field1", "field3")
    table1.alias shouldBe TableRegistrar.aliasFor("Test")

    val table2: Table[Test, TestKey] = Table.derive[Test, TestKey](
      Some("test_name"),
      Some("test_schema"),
      Map("field1" -> "field_1", "field2" -> "field_2"),
      str => str.toUpperCase
    )

    table2.name shouldBe "test_name"
    table2.schema shouldBe Some("test_schema")
    table2.fieldConverter shouldEqual Map("field1" -> "field_1", "field2" -> "field_2")
    table2.fieldMapper("lower") shouldBe "LOWER"
    table2.fields shouldBe List("field1", "field2", "field3", "field4")
    table2.keyFields shouldBe List("field1", "field3")
    table2.alias shouldBe TableRegistrar.aliasFor("Test")
  }

  it should "provide a valid Unit instance" in {
    Table.unit.name shouldBe "unit"
    Table.unit.schema shouldBe empty
    Table.unit.fields shouldBe empty
    Table.unit.keyFields shouldBe empty
    Table.unit.alias shouldBe "x0"
  }
}
