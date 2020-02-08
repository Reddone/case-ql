package com.github.reddone.caseql.sql.table

import java.sql.Timestamp
import java.time.Instant

import com.github.reddone.caseql.sql.filter.models._
import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.table.query.Action
import com.github.reddone.caseql.sql.modifier.models._
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import doobie._
import doobie.implicits._
import javasql._
import javatime._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TableQuerySpec extends AnyFlatSpec with Matchers {

  // test model
  case class Test(
      field1: Int,
      field2: Option[String],
      field3: Long,
      field4: Option[Timestamp]
  )
  // test key
  case class TestKey(
      field1: Int,
      field3: Long
  )
  // test filter
  case class TestFilter(
      field1: Option[IntFilter],
      field2: Option[StringFilterOption],
      field3: Option[LongFilter],
      field4: Option[TimestampFilterOption],
      AND: Option[Seq[TestFilter]],
      OR: Option[Seq[TestFilter]],
      NOT: Option[TestFilter]
  ) extends EntityFilter[TestFilter]
  // test modifier
  case class TestModifier(
      field1: Option[IntModifier],
      field2: Option[StringModifierOption],
      field3: Option[LongModifier],
      field4: Option[TimestampModifierOption]
  ) extends EntityModifier[TestModifier]

  implicit val table: Table[Test, TestKey]                      = Table.derive[Test, TestKey]()
  val syntax: TableSyntax[Test]                                 = table.syntax("t")
  implicit val tableFilter: TableFilter[Test, TestFilter]       = TableFilter.derive[Test, TestFilter]()
  implicit val tableModifier: TableModifier[Test, TestModifier] = TableModifier.derive[Test, TestModifier]()

  "TableQuery filterFragment" should "work with an empty filter" in {
    val filter1 = TestFilter(
      None,
      None,
      None,
      None,
      None,
      None,
      None
    )

    table.byFilterFragment(filter1, None) shouldBe None

    val filter2 = TestFilter(
      Some(IntFilter.empty),
      Some(StringFilterOption.empty),
      Some(LongFilter.empty),
      Some(TimestampFilterOption.empty),
      None,
      None,
      None
    )

    table.byFilterFragment(filter2, None) shouldBe None
  }

  it should "work with a flat filter" in {
    val filter1 = TestFilter(
      Some(IntFilter.empty.copy(EQ = Some(1), IN = Some(Seq(1, 1, 1)))),
      Some(StringFilterOption.empty.copy(EQ = Some("2"), CONTAINS = Some("2"))),
      Some(LongFilter.empty.copy(EQ = Some(3L), IN = Some(Seq(3L, 3L, 3L)))),
      Some(TimestampFilterOption.empty.copy(EQ = Some(Timestamp.from(Instant.EPOCH)))),
      None,
      None,
      None
    )

    val expected = "Fragment(\"" +
      "(" +
      "((t.field1 = ? ) AND (t.field1 IN (?, ?, ?) ) ) AND " +
      "((t.field2 = ? ) AND (t.field2 LIKE %?% ) ) AND " +
      "((t.field3 = ? ) AND (t.field3 IN (?, ?, ?) ) ) AND " +
      "((t.field4 = ? ) ) " +
      ") " +
      "\")"
    val result = table.byFilterFragment(filter1, None)

    result shouldBe defined
    result.get.toString shouldBe expected
  }

  it should "work with a level one depth filter" in {
    val filter1 = TestFilter(
      Some(IntFilter.empty.copy(EQ = Some(11))),
      Some(StringFilterOption.empty.copy(EQ = Some("22"))),
      None,
      None,
      None,
      None,
      None
    )

    val filter2 = TestFilter(
      Some(IntFilter.empty.copy(EQ = Some(1))),
      Some(StringFilterOption.empty.copy(EQ = Some("2"))),
      None,
      None,
      AND = Some(Seq(filter1, filter1)),
      OR = Some(Seq(filter1, filter1)),
      NOT = Some(filter1)
    )

    val expected = "Fragment(\"" +
      "(((t.field1 = ? ) ) AND ((t.field2 = ? ) ) ) " +
      "AND " +
      "((" +
      "(((t.field1 = ? ) ) AND ((t.field2 = ? ) ) ) " +
      ") AND (" +
      "(((t.field1 = ? ) ) AND ((t.field2 = ? ) ) ) " +
      ") ) " +
      "AND " +
      "((" +
      "(((t.field1 = ? ) ) AND ((t.field2 = ? ) ) ) " +
      ") OR (" +
      "(((t.field1 = ? ) ) AND ((t.field2 = ? ) ) ) " +
      ") ) " +
      "AND " +
      "(NOT (" +
      "(((t.field1 = ? ) ) AND ((t.field2 = ? ) ) ) " +
      ") ) " +
      "\")"
    val result = table.byFilterFragment(filter2, None)

    result shouldBe defined
    result.get.toString shouldBe expected
  }

  it should "work with an any level depth filter" in {
    val filter1 = TestFilter(
      None,
      None,
      None,
      None,
      None,
      Some(
        Seq(
          TestFilter(Some(IntFilter.empty.copy(EQ = Some(11))), None, None, None, None, None, None),
          TestFilter(None, Some(StringFilterOption.empty.copy(EQ = Some("22"))), None, None, None, None, None),
          TestFilter(
            None,
            None,
            None,
            None,
            Some(
              Seq(
                TestFilter(
                  None,
                  None,
                  Some(LongFilter.empty.copy(EQ = Some(333L), IN = Some(Seq(333L, 333L, 333L)))),
                  Some(TimestampFilterOption.empty.copy(EQ = Some(Timestamp.from(Instant.EPOCH)))),
                  None,
                  None,
                  None
                ),
                TestFilter(
                  None,
                  None,
                  None,
                  None,
                  None,
                  None,
                  Some(
                    TestFilter(
                      None,
                      Some(StringFilterOption.empty.copy(CONTAINS = Some("2222"))),
                      None,
                      None,
                      None,
                      None,
                      None
                    )
                  )
                ),
                TestFilter(
                  None,
                  None,
                  None,
                  None,
                  None,
                  None,
                  Some(
                    TestFilter(
                      Some(IntFilter.empty.copy(IN = Some(Seq(1111, 1111)))),
                      None,
                      None,
                      None,
                      None,
                      None,
                      None
                    )
                  )
                )
              )
            ),
            None,
            None
          )
        )
      ),
      None
    )

    val expected = "Fragment(\"" +
      "((" +
      "(((t.field1 = ? ) ) ) " +
      ") OR (" +
      "(((t.field2 = ? ) ) ) " +
      ") OR (" +
      "((" +
      "(((t.field3 = ? ) AND (t.field3 IN (?, ?, ?) ) ) AND ((t.field4 = ? ) ) ) " +
      ") AND (" +
      "(NOT ((((t.field2 LIKE %?% ) ) ) ) ) " +
      ") AND (" +
      "(NOT ((((t.field1 IN (?, ?) ) ) ) ) ) " +
      ") ) " +
      ") ) " +
      "\")"
    val result = table.byFilterFragment(filter1, None)

    result shouldBe defined
    result.get.toString shouldBe expected
  }

  "TableQuery selectFragment" should "work correctly" in {
    val filter1 = TestFilter(
      Some(IntFilter.empty.copy(EQ = Some(1))),
      None,
      None,
      None,
      None,
      None,
      None
    )

    val expected = "Fragment(\"" +
      "SELECT t.field1, t.field2, t.field3, t.field4 " +
      "FROM test t " +
      "WHERE (((t.field1 = ? ) ) ) " +
      "\")"
    val result = table.select(filter1).toFragment

    result.toString shouldBe expected
  }

  "TableQuery insertFragment" should "work correctly" in {
    val modifier1 = TestModifier(
      Some(IntModifier(ModifierAction.Set, Some(1))),
      Some(StringModifierOption(ModifierOptionAction.Null, None)),
      Some(LongModifier(ModifierAction.Default, None)),
      None
    )

    val expected = "Fragment(\"" +
      "INSERT INTO test (field1, field2, field3) " +
      "VALUES (? , ? , DEFAULT ) " +
      "\")"
    val result = table.insertOne(modifier1).toFragment

    result.toString shouldBe expected
  }

  "TableQuery updateFragment" should "work correctly" in {
    val modifier1 = TestModifier(
      Some(IntModifier(ModifierAction.Set, Some(1))),
      Some(StringModifierOption(ModifierOptionAction.Null, None)),
      Some(LongModifier(ModifierAction.Default, None)),
      None
    )
    val filter1 = TestFilter(
      Some(IntFilter.empty.copy(EQ = Some(1))),
      None,
      None,
      None,
      None,
      None,
      None
    )

    val expected = "Fragment(\"" +
      "UPDATE test " +
      "SET field1 = ? , field2 = ? , field3 = DEFAULT " +
      "WHERE (((field1 = ? ) ) ) " +
      "\")"
    val result = table.update(modifier1, filter1).toFragment

    result.toString shouldBe expected
  }

  "TableQuery deleteFragment" should "work correctly" in {
    val filter1 = TestFilter(
      Some(IntFilter.empty.copy(EQ = Some(1))),
      None,
      None,
      None,
      None,
      None,
      None
    )

    val expected = "Fragment(\"" +
      "DELETE FROM test " +
      "WHERE (((field1 = ? ) ) ) " +
      "\")"
    val result = table.delete(filter1).toFragment

    result.toString shouldBe expected
  }
}