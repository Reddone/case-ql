package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.model.db.{Test, TestKey}
import doobie._
import doobie.implicits._
import javasql._
import javatime._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TableSyntaxSpec extends AnyFlatSpec with Matchers {

  val table1: Table[Test, TestKey] = Table.derive[Test, TestKey](useTableAlias = false)
  val table2: Table[Test, TestKey] = Table.derive[Test, TestKey](
    Some("test_name"),
    Some("test_schema"),
    Map("field1" -> "field_1", "field2" -> "field_2"),
    str => str.toUpperCase,
    useTableAlias = false
  )

  "TableSyntax implicit resolution" should "compile" in {
    implicit val table: Table[Test, TestKey] = table1
    """implicitly[TableSyntax[Test]]""" should compile
  }

  it should "not compile" in {
    """implicitly[TableSyntax[Test]]""" shouldNot compile
  }

  "TableSyntax class" should "work correctly" in {
    val syntax1: TableSyntax[Test] = table1.syntax

    syntax1.name shouldBe "test"
    syntax1.fullName shouldBe "test"
    syntax1.columns shouldBe List(
      "field1",
      "field2",
      "field3",
      "field4"
    )
    syntax1.selectionColumns shouldBe List(
      "test.field1",
      "test.field2",
      "test.field3",
      "test.field4"
    )
    syntax1.keyColumns shouldBe List(
      "field1",
      "field3"
    )
    syntax1.selectionKeyColumns shouldBe List(
      "test.field1",
      "test.field3"
    )
    syntax1.column("field1") shouldBe "field1"
    syntax1.selectionColumn("field1") shouldBe "test.field1"

    val syntax2 = table2.syntax

    syntax2.name shouldBe "test_name"
    syntax2.fullName shouldBe "test_schema.test_name"
    syntax2.columns shouldBe List(
      "field_1",
      "field_2",
      "FIELD3",
      "FIELD4"
    )
    syntax2.selectionColumns shouldBe List(
      "test_name.field_1",
      "test_name.field_2",
      "test_name.FIELD3",
      "test_name.FIELD4"
    )
    syntax2.keyColumns shouldBe List(
      "field_1",
      "FIELD3"
    )
    syntax2.selectionKeyColumns shouldBe List(
      "test_name.field_1",
      "test_name.FIELD3"
    )
    syntax2.column("field1") shouldBe "field_1"
    syntax2.selectionColumn("field1") shouldBe "test_name.field_1"

    val syntax3: TableSyntax[Test] = table2.syntax.withAlias("t")

    syntax3.name shouldBe "t"
    syntax3.fullName shouldBe "test_schema.test_name t"
    syntax3.columns shouldBe List(
      "field_1",
      "field_2",
      "FIELD3",
      "FIELD4"
    )
    syntax3.selectionColumns shouldBe List(
      "t.field_1",
      "t.field_2",
      "t.FIELD3",
      "t.FIELD4"
    )
    syntax3.keyColumns shouldBe List(
      "field_1",
      "FIELD3"
    )
    syntax3.selectionKeyColumns shouldBe List(
      "t.field_1",
      "t.FIELD3"
    )
    syntax3.column("field1") shouldBe "field_1"
    syntax3.selectionColumn("field1") shouldBe "t.field_1"
  }
}
