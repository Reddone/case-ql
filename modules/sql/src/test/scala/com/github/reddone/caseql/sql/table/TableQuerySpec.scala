package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.TestModel.{Test, TestKey, TestFilter, TestModifier}
import com.github.reddone.caseql.sql.filter.models._
import com.github.reddone.caseql.sql.modifier.models._
import doobie._
import doobie.implicits._
import javasql._
import javatime._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TableQuerySpec extends AnyFlatSpec with Matchers {

  implicit val table: Table[Test, TestKey]                      = Table.derive[Test, TestKey]()
  implicit val tableFilter: TableFilter[Test, TestFilter]       = TableFilter.derive[Test, TestFilter]()
  implicit val tableModifier: TableModifier[Test, TestModifier] = TableModifier.derive[Test, TestModifier]()

  "TableQuery" should "produce a correct fragment on select" in {
    val filter = TestFilter.empty.copy(
      field1 = Some(IntFilter.empty.copy(EQ = Some(1)))
    )

    val result1 = table.select(filter, Some("a1"))

    result1.toFragment.toString shouldBe "Fragment(\"" +
      "SELECT a1.field1, a1.field2, a1.field3, a1.field4 " +
      "FROM test a1 " +
      "WHERE (((a1.field1 = ? ) ) ) " +
      "\")"

    val result2 = table.select(filter, None)

    result2.toFragment.toString shouldBe "Fragment(\"" +
      "SELECT field1, field2, field3, field4 " +
      "FROM test " +
      "WHERE (((field1 = ? ) ) ) " +
      "\")"
  }

  it should "produce a correct fragment on selectByKey" in {
    val key = TestKey(1, 2L)

    val result1 = table.selectByKey(key, Some("a1"))

    result1.toFragment.toString shouldBe "Fragment(\"" +
      "SELECT a1.field1, a1.field2, a1.field3, a1.field4 " +
      "FROM test a1 " +
      "WHERE a1.field1 = ? AND a1.field3 = ?" +
      "\")"

    val result2 = table.selectByKey(key, None)

    result2.toFragment.toString shouldBe "Fragment(\"" +
      "SELECT field1, field2, field3, field4 " +
      "FROM test " +
      "WHERE field1 = ? AND field3 = ?" +
      "\")"
  }

  it should "produce a correct fragment in insert" in {
    val modifier = TestModifier(
      Some(IntModifier(ModifierAction.Set, Some(1))),
      Some(StringModifierOption(ModifierOptionAction.Null, None)),
      Some(LongModifier(ModifierAction.Default, None)),
      None
    )
    val result1 = table.insert(modifier)

    result1.toFragment.toString shouldBe "Fragment(\"" +
      "INSERT INTO test (field1, field2, field3) " +
      "VALUES (? , ? , DEFAULT ) " +
      "\")"
  }

  it should "produce a correct fragment in insertReturningKey" in {
    val modifier = TestModifier(
      Some(IntModifier(ModifierAction.Set, Some(1))),
      Some(StringModifierOption(ModifierOptionAction.Null, None)),
      Some(LongModifier(ModifierAction.Default, None)),
      None
    )
    val result1 = table.insertReturningKey(modifier)

    result1.toFragment.toString shouldBe "Fragment(\"" +
      "INSERT INTO test (field1, field2, field3) " +
      "VALUES (? , ? , DEFAULT ) " +
      "\")"
  }

  it should "produce a correct fragment in update" in {
    val modifier = TestModifier(
      Some(IntModifier(ModifierAction.Set, Some(1))),
      Some(StringModifierOption(ModifierOptionAction.Null, None)),
      Some(LongModifier(ModifierAction.Default, None)),
      None
    )
    val filter = TestFilter.empty.copy(
      field1 = Some(IntFilter.empty.copy(EQ = Some(1)))
    )

    val result1 = table.update(modifier, filter)

    result1.toFragment.toString shouldBe "Fragment(\"" +
      "UPDATE test " +
      "SET field1 = ? , field2 = ? , field3 = DEFAULT " +
      "WHERE (((field1 = ? ) ) ) " +
      "\")"
  }

  it should "produce a correct fragment in updateReturningKeys" in {
    val modifier = TestModifier(
      Some(IntModifier(ModifierAction.Set, Some(1))),
      Some(StringModifierOption(ModifierOptionAction.Null, None)),
      Some(LongModifier(ModifierAction.Default, None)),
      None
    )
    val filter = TestFilter.empty.copy(
      field1 = Some(IntFilter.empty.copy(EQ = Some(1)))
    )

    val result1 = table.update(modifier, filter)

    result1.toFragment.toString shouldBe "Fragment(\"" +
      "UPDATE test " +
      "SET field1 = ? , field2 = ? , field3 = DEFAULT " +
      "WHERE (((field1 = ? ) ) ) " +
      "\")"
  }

  it should "produce a correct fragment in updateByKey" in {
    val modifier = TestModifier(
      Some(IntModifier(ModifierAction.Set, Some(1))),
      Some(StringModifierOption(ModifierOptionAction.Null, None)),
      Some(LongModifier(ModifierAction.Default, None)),
      None
    )
    val key = TestKey(1, 2L)

    val result1 = table.updateByKey(modifier, key)

    result1.toFragment.toString shouldBe "Fragment(\"" +
      "UPDATE test " +
      "SET field1 = ? , field2 = ? , field3 = DEFAULT " +
      "WHERE field1 = ? AND field3 = ?" +
      "\")"
  }

  it should "produce a correct fragment in updateByKeyReturningKeys" in {
    val modifier = TestModifier(
      Some(IntModifier(ModifierAction.Set, Some(1))),
      Some(StringModifierOption(ModifierOptionAction.Null, None)),
      Some(LongModifier(ModifierAction.Default, None)),
      None
    )
    val key = TestKey(1, 2L)

    val result1 = table.updateByKey(modifier, key)

    result1.toFragment.toString shouldBe "Fragment(\"" +
      "UPDATE test " +
      "SET field1 = ? , field2 = ? , field3 = DEFAULT " +
      "WHERE field1 = ? AND field3 = ?" +
      "\")"
  }

  it should "produce a correct fragment in delete" in {
    val filter = TestFilter.empty.copy(
      field1 = Some(IntFilter.empty.copy(EQ = Some(1)))
    )

    val result1 = table.delete(filter)

    result1.toFragment.toString shouldBe "Fragment(\"" +
      "DELETE FROM test " +
      "WHERE (((field1 = ? ) ) ) " +
      "\")"
  }

  it should "produce a correct fragment in deleteReturningKeys" in {
    val filter = TestFilter.empty.copy(
      field1 = Some(IntFilter.empty.copy(EQ = Some(1)))
    )

    val result1 = table.delete(filter)

    result1.toFragment.toString shouldBe "Fragment(\"" +
      "DELETE FROM test " +
      "WHERE (((field1 = ? ) ) ) " +
      "\")"
  }

  it should "produce a correct fragment in deleteByKey" in {
    val key = TestKey(1, 2L)

    val result1 = table.deleteByKey(key)

    result1.toFragment.toString shouldBe "Fragment(\"" +
      "DELETE FROM test " +
      "WHERE field1 = ? AND field3 = ?" +
      "\")"
  }

  it should "produce a correct fragment in deleteByKeyReturningKeys" in {
    val key = TestKey(1, 2L)

    val result1 = table.deleteByKey(key)

    result1.toFragment.toString shouldBe "Fragment(\"" +
      "DELETE FROM test " +
      "WHERE field1 = ? AND field3 = ?" +
      "\")"
  }
}
