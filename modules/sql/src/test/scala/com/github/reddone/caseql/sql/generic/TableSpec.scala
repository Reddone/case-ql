package com.github.reddone.caseql.sql.generic

import java.sql.Timestamp

import doobie._
import doobie.implicits._
import javasql._
import javatime._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import shapeless.test.illTyped

class TableSpec extends AnyFlatSpec with Matchers {

  // test model
  case class Test(
      field1: Int,
      field2: Option[String],
      field3: Long,
      field4: Option[Timestamp]
  )
  // simple case, should compile
  case class TestKey(
      field1: Int,
      field3: Long
  )
  // simple case but unordered, should compile
  case class TestKeyUnordered(
      field3: Long,
      field1: Int
  )
  // with other field, should not compile
  case class TestKeyOther(
      field1: Int,
      field3: Long,
      field5: Option[String]
  )
  // with other field and unordered, should not compile
  case class TestKeyOtherUnordered(
      field5: Option[String],
      field3: Long,
      field1: Int
  )

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
    val syntax1: table1.Syntax       = table1.defaultSyntax

    table1.name shouldBe "test"
    table1.schema shouldBe None
    table1.fieldConverter.isEmpty shouldBe true
    table1.fieldMapper("CamelCase") shouldBe "camel_case"
    table1.fields shouldBe List("field1", "field2", "field3", "field4")
    table1.keyFields shouldBe List("field1", "field3")

    syntax1.name shouldBe "test"
    syntax1.columns shouldBe List("field1", "field2", "field3", "field4")
    syntax1.column("field1") shouldBe "field1"
    syntax1.field1 shouldBe "field1"

    val table2: Table[Test, TestKey] = Table.derive[Test, TestKey](
      Some("test_name"),
      Some("test_schema"),
      Map("field1" -> "field_1", "field2" -> "field_2"),
      str => str.toUpperCase
    )
    val syntax2 = table2.syntax("t")

    table2.name shouldBe "test_name"
    table2.schema shouldBe Some("test_schema")
    table2.fieldConverter shouldEqual Map("field1" -> "field_1", "field2" -> "field_2")
    table2.fieldMapper("lower") shouldBe "LOWER"
    table2.fields shouldBe List("field1", "field2", "field3", "field4")
    table2.keyFields shouldBe List("field1", "field3")

    syntax2.name shouldBe "test_schema.test_name t"
    syntax2.columns shouldBe List("t.field_1", "t.field_2", "t.FIELD3", "t.FIELD4")
    syntax2.column("field1") shouldBe "t.field_1"
    syntax2.field1 shouldBe "t.field_1"
  }
}
