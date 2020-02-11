package com.github.reddone.caseql.sql.table

import java.sql.Timestamp
import java.time.Instant

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
    val filter1 = TestFilter(
      Some(IntFilter.empty.copy(EQ = Some(1))),
      None,
      None,
      None,
      None,
      None,
      None
    )
    val alias1 = "a1"
    val result1 = table.select(filter1, Some(alias1))

    result1.toFragment.toString shouldBe "Fragment(\"" +
      "SELECT t.field1, t.field2, t.field3, t.field4 " +
      "FROM test t " +
      "WHERE (((t.field1 = ? ) ) ) " +
      "\")"
  }

  it should "produce a correct fragment on selectByKey" in {

  }

  it should "produce a correct fragment in insert" in {

  }

  it should "produce a correct fragment in insertReturningKey" in {

  }

  it should "produce a correct fragment in update" in {

  }

  it should "produce a correct fragment in updateReturningKeys" in {

  }

  it should "produce a correct fragment in updateByKey" in {

  }

  it should "produce a correct fragment in updateByKeyReturningKeys" in {

  }

  it should "produce a correct fragment in delete" in {

  }

  it should "produce a correct fragment in deleteReturningKeys" in {

  }

  it should "produce a correct fragment in deleteByKey" in {

  }

  it should "produce a correct fragment in deleteByKeyReturningKeys" in {

  }

//  "TableQuery insertFragment" should "work correctly" in {
//    val modifier1 = TestModifier(
//      Some(IntModifier(ModifierAction.Set, Some(1))),
//      Some(StringModifierOption(ModifierOptionAction.Null, None)),
//      Some(LongModifier(ModifierAction.Default, None)),
//      None
//    )
//
//    val expected = "Fragment(\"" +
//      "INSERT INTO test (field1, field2, field3) " +
//      "VALUES (? , ? , DEFAULT ) " +
//      "\")"
//    val result = table.insertOne(modifier1).toFragment
//
//    result.toString shouldBe expected
//  }
//
//  "TableQuery updateFragment" should "work correctly" in {
//    val modifier1 = TestModifier(
//      Some(IntModifier(ModifierAction.Set, Some(1))),
//      Some(StringModifierOption(ModifierOptionAction.Null, None)),
//      Some(LongModifier(ModifierAction.Default, None)),
//      None
//    )
//    val filter1 = TestFilter(
//      Some(IntFilter.empty.copy(EQ = Some(1))),
//      None,
//      None,
//      None,
//      None,
//      None,
//      None
//    )
//
//    val expected = "Fragment(\"" +
//      "UPDATE test " +
//      "SET field1 = ? , field2 = ? , field3 = DEFAULT " +
//      "WHERE (((field1 = ? ) ) ) " +
//      "\")"
//    val result = table.update(modifier1, filter1).toFragment
//
//    result.toString shouldBe expected
//  }
//
//  "TableQuery deleteFragment" should "work correctly" in {
//    val filter1 = TestFilter(
//      Some(IntFilter.empty.copy(EQ = Some(1))),
//      None,
//      None,
//      None,
//      None,
//      None,
//      None
//    )
//
//    val expected = "Fragment(\"" +
//      "DELETE FROM test " +
//      "WHERE (((field1 = ? ) ) ) " +
//      "\")"
//    val result = table.delete(filter1).toFragment
//
//    result.toString shouldBe expected
//  }
}
