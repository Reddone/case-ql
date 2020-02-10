package com.github.reddone.caseql.sql.table

import java.sql.Timestamp

import com.github.reddone.caseql.sql.filter.models._
import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import doobie._
import doobie.implicits._
import javasql._
import javatime._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import shapeless.test.illTyped

class TableFilterSpec extends AnyFlatSpec with Matchers {

  // test model
  case class Test(
      field1: Int,
      field2: Option[String],
      field3: Long,
      field4: Option[Timestamp]
  )
  case class TestKey(
      field1: Int,
      field3: Long
  )
  // simple case, should compile
  case class TestFilter(
      field1: Option[IntFilter],
      field2: Option[StringFilterOption],
      field3: Option[LongFilter],
      field4: Option[TimestampFilterOption],
      AND: Option[Seq[TestFilter]],
      OR: Option[Seq[TestFilter]],
      NOT: Option[TestFilter]
  ) extends EntityFilter[TestFilter]
  object TestFilter {
    val empty: TestFilter = TestFilter(None, None, None, None, None, None, None)
  }
  // simple case but unordered, should compile
  case class TestFilterUnordered(
      field4: Option[TimestampFilterOption],
      field2: Option[StringFilterOption],
      field3: Option[LongFilter],
      field1: Option[IntFilter],
      AND: Option[Seq[TestFilterUnordered]],
      OR: Option[Seq[TestFilterUnordered]],
      NOT: Option[TestFilterUnordered]
  ) extends EntityFilter[TestFilterUnordered]
  object TestFilterUnordered {
    val empty: TestFilterUnordered = TestFilterUnordered(None, None, None, None, None, None, None)
  }
  // with other fields, should compile
  case class TestFilterOther(
      field1: Option[IntFilter],
      field2: Option[StringFilterOption],
      field3: Option[LongFilter],
      field4: Option[TimestampFilterOption],
      otherField1: String,
      otherField2: Seq[Int],
      AND: Option[Seq[TestFilterOther]],
      OR: Option[Seq[TestFilterOther]],
      NOT: Option[TestFilterOther]
  ) extends EntityFilter[TestFilterOther]
  object TestFilterOther {
    val empty: TestFilterOther = TestFilterOther(None, None, None, None, "", Seq.empty, None, None, None)
  }
  // with other fields and unordered, should compile
  case class TestFilterOtherUnordered(
      otherField2: Seq[Int],
      field4: Option[TimestampFilterOption],
      field2: Option[StringFilterOption],
      field3: Option[LongFilter],
      otherField1: String,
      field1: Option[IntFilter],
      AND: Option[Seq[TestFilterOtherUnordered]],
      OR: Option[Seq[TestFilterOtherUnordered]],
      NOT: Option[TestFilterOtherUnordered]
  ) extends EntityFilter[TestFilterOtherUnordered]
  object TestFilterOtherUnordered {
    val empty: TestFilterOtherUnordered =
      TestFilterOtherUnordered(Seq.empty, None, None, None, "", None, None, None, None)
  }
  // one more field, should not compile
  case class TestFilterPlus(
      field1: Option[IntFilter],
      field2: Option[StringFilterOption],
      field3: Option[LongFilter],
      field4: Option[TimestampFilterOption],
      field5: Option[StringFilter],
      AND: Option[Seq[TestFilterPlus]],
      OR: Option[Seq[TestFilterPlus]],
      NOT: Option[TestFilterPlus]
  ) extends EntityFilter[TestFilterPlus]
  object TestFilterPlus {
    val empty: TestFilterPlus = TestFilterPlus(None, None, None, None, None, None, None, None)
  }
  // one more field and unordered, should not compile
  case class TestFilterPlusUnordered(
      field1: Option[IntFilter],
      field5: Option[StringFilter],
      field4: Option[TimestampFilterOption],
      field2: Option[StringFilterOption],
      field3: Option[LongFilter],
      AND: Option[Seq[TestFilterPlusUnordered]],
      OR: Option[Seq[TestFilterPlusUnordered]],
      NOT: Option[TestFilterPlusUnordered]
  ) extends EntityFilter[TestFilterPlusUnordered]
  object TestFilterPlusUnordered {
    val empty: TestFilterPlusUnordered = TestFilterPlusUnordered(None, None, None, None, None, None, None, None)
  }
  // one less field, should not compile
  case class TestFilterLess(
      field1: Option[IntFilter],
      field2: Option[StringFilterOption],
      field3: Option[LongFilter],
      AND: Option[Seq[TestFilterLess]],
      OR: Option[Seq[TestFilterLess]],
      NOT: Option[TestFilterLess]
  ) extends EntityFilter[TestFilterLess]
  object TestFilterLess {
    val empty: TestFilterLess = TestFilterLess(None, None, None, None, None, None)
  }
  // one less field and unordered, should not compile
  case class TestFilterLessUnordered(
      field2: Option[StringFilterOption],
      field1: Option[IntFilter],
      field3: Option[LongFilter],
      AND: Option[Seq[TestFilterLessUnordered]],
      OR: Option[Seq[TestFilterLessUnordered]],
      NOT: Option[TestFilterLessUnordered]
  ) extends EntityFilter[TestFilterLessUnordered]
  object TestFilterLessUnordered {
    val empty: TestFilterLessUnordered = TestFilterLessUnordered(None, None, None, None, None, None)
  }

  implicit val table: Table[Test, TestKey] = Table.derive[Test, TestKey]()

  "TableFilter derivation" should "compile in the simple case" in {
    """TableFilter.derive[Test, TestFilter]()""" should compile
  }

  it should "compile in the unordered case" in {
    """TableFilter.derive[Test, TestFilterUnordered]()""" should compile
  }

  it should "compile in the other case" in {
    """TableFilter.derive[Test, TestFilterOther]()""" should compile
  }

  it should "compile in the other unordered case" in {
    """TableFilter.derive[Test, TestFilterOtherUnordered]()""" should compile
  }

  it should "not compile in the plus case" in {
    """TableFilter.derive[Test, TestFilterPlus]()""" shouldNot compile
    illTyped { """TableFilter.derive[Test, TestFilterPlus]()""" }
  }

  it should "not compile in the plus unordered case" in {
    """TableFilter.derive[Test, TestFilterPlusUnordered]()""" shouldNot compile
    illTyped { """TableFilter.derive[Test, TestFilterPlusUnordered]()""" }
  }

  it should "not compile in the less case" in {
    """TableFilter.derive[Test, TestFilterLess]()""" shouldNot compile
    illTyped { """TableFilter.derive[Test, TestFilterLess]()""" }
  }

  it should "not compile in the less unordered case" in {
    """TableFilter.derive[Test, TestFilterLessUnordered]()""" shouldNot compile
    illTyped { """TableFilter.derive[Test, TestFilterLessUnordered]()""" }
  }

  "TableFilter typeclass" should "work correctly" in {
    val tableFilter1: TableFilter[Test, TestFilter] =
      TableFilter.derive[Test, TestFilter]()
    val filter1 = TestFilter.empty.copy(
      field1 = Some(IntFilter.empty.copy(EQ = Some(1), IN = Some(Seq(2, 3)))),
      field2 = Some(StringFilterOption.empty.copy(CONTAINS = Some("A")))
    )
    val alias1  = "a1"
    val syntax1 = table.syntax.withAlias(Some(alias1))
    val result1 = tableFilter1.entityFilterFragments(filter1)

    result1(Some(alias1)).map(_.toString) shouldBe List(

    )


//    val tableFilter2: TableFilter[Test, TestFilterUnordered] =
//      TableFilter.derive[Test, TestFilterUnordered]()
//    val filter2 = TestFilterUnordered(
//      None,
//      Some(StringFilterOption.empty),
//      None,
//      Some(IntFilter.empty)
//    )
//
//    tableFilter2.keys() shouldBe List('field4, 'field2, 'field3, 'field1)
//    tableFilter2.values(filter2) shouldBe List(filter2.field4, filter2.field2, filter2.field3, filter2.field1)
//
//    val tableFilter3: TableFilter[Test, TestFilterOther] =
//      TableFilter.derive[Test, TestFilterOther]()
//    val filter3 = TestFilterOther(
//      Some(IntFilter.empty),
//      Some(StringFilterOption.empty),
//      None,
//      None,
//      "5",
//      Seq(6)
//    )
//
//    tableFilter3.keys() shouldBe List('field1, 'field2, 'field3, 'field4)
//    tableFilter3.values(filter3) shouldBe List(filter3.field1, filter3.field2, filter3.field3, filter3.field4)
//
//    val tableFilter4: TableFilter[Test, TestFilterOtherUnordered] =
//      TableFilter.derive[Test, TestFilterOtherUnordered]()
//    val filter4 = TestFilterOtherUnordered(
//      Seq(6),
//      None,
//      Some(StringFilterOption.empty),
//      None,
//      "5",
//      Some(IntFilter.empty)
//    )
//
//    tableFilter4.keys() shouldBe List('field4, 'field2, 'field3, 'field1)
//    tableFilter4.values(filter4) shouldBe List(filter4.field4, filter4.field2, filter4.field3, filter4.field1)
  }

  // TODO: test relation filters
}
