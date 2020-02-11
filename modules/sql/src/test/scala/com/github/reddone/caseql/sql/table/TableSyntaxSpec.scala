package com.github.reddone.caseql.sql.table

import java.sql.Timestamp

import com.github.reddone.caseql.sql.TestModel.{Test, TestKey}
import doobie._
import doobie.implicits._
import javasql._
import javatime._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TableSyntaxSpec extends AnyFlatSpec with Matchers {

  val table1: Table[Test, TestKey] = Table.derive[Test, TestKey]()
  val table2: Table[Test, TestKey] = Table.derive[Test, TestKey](
    Some("test_name"),
    Some("test_schema"),
    Map("field1" -> "field_1", "field2" -> "field_2"),
    str => str.toUpperCase
  )

  "TableSyntax implicit resolution" should "compile" in {
    implicit val table: Table[Test, TestKey] = table1
    """implicitly[TableSyntax[Test]]""" should compile
  }

  it should "not compile" in {
    """implicitly[TableSyntax[Test]]""" shouldNot compile
  }

  "TableSyntax methods" should "work correctly" in {
    val alias1                     = table1.alias
    val syntax1: TableSyntax[Test] = table1.syntax

    syntax1.name shouldBe "test"
    syntax1.aliasedName shouldBe s"test $alias1"
    syntax1.columns shouldBe List(s"$alias1.field1", s"$alias1.field2", s"$alias1.field3", s"$alias1.field4")
    syntax1.column("field1") shouldBe s"$alias1.field1"
    syntax1.field1 shouldBe s"$alias1.field1"

    val syntax2: TableSyntax[Test] = table2.syntax.withAlias(Some("t"))

    syntax2.name shouldBe "test_schema.test_name"
    syntax2.aliasedName shouldBe "test_schema.test_name t"
    syntax2.columns shouldBe List("t.field_1", "t.field_2", "t.FIELD3", "t.FIELD4")
    syntax2.column("field1") shouldBe "t.field_1"
    syntax2.field1 shouldBe "t.field_1"
  }
}
