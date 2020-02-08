package com.github.reddone.caseql.sql.table

import java.sql.Timestamp

import com.github.reddone.caseql.sql.filter.models._
import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
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
  // simple case, should compile
  case class TestFilter(
      field1: Option[IntFilter],
      field2: Option[StringFilterOption],
      field3: Option[LongFilter],
      field4: Option[TimestampFilterOption]
  ) extends EntityFilter[TestFilter] {
    override def AND: Option[Seq[TestFilter]] = None
    override def OR: Option[Seq[TestFilter]]  = None
    override def NOT: Option[TestFilter]      = None
  }
  // simple case but unordered, should compile
  case class TestFilterUnordered(
      field4: Option[TimestampFilterOption],
      field2: Option[StringFilterOption],
      field3: Option[LongFilter],
      field1: Option[IntFilter]
  ) extends EntityFilter[TestFilterUnordered] {
    override def AND: Option[Seq[TestFilterUnordered]] = None
    override def OR: Option[Seq[TestFilterUnordered]]  = None
    override def NOT: Option[TestFilterUnordered]      = None
  }
  // with other fields, should compile
  case class TestFilterOther(
      field1: Option[IntFilter],
      field2: Option[StringFilterOption],
      field3: Option[LongFilter],
      field4: Option[TimestampFilterOption],
      otherField1: String,
      otherField2: Seq[Int]
  ) extends EntityFilter[TestFilterOther] {
    override def AND: Option[Seq[TestFilterOther]] = None
    override def OR: Option[Seq[TestFilterOther]]  = None
    override def NOT: Option[TestFilterOther]      = None
  }
  // with other fields and unordered, should compile
  case class TestFilterOtherUnordered(
      otherField2: Seq[Int],
      field4: Option[TimestampFilterOption],
      field2: Option[StringFilterOption],
      field3: Option[LongFilter],
      otherField1: String,
      field1: Option[IntFilter]
  ) extends EntityFilter[TestFilterOtherUnordered] {
    override def AND: Option[Seq[TestFilterOtherUnordered]] = None
    override def OR: Option[Seq[TestFilterOtherUnordered]]  = None
    override def NOT: Option[TestFilterOtherUnordered]      = None
  }
  // one more field, should not compile
  case class TestFilterPlus(
      field1: Option[IntFilter],
      field2: Option[StringFilterOption],
      field3: Option[LongFilter],
      field4: Option[TimestampFilterOption],
      field5: Option[StringFilter]
  ) extends EntityFilter[TestFilterPlus] {
    override def AND: Option[Seq[TestFilterPlus]] = None
    override def OR: Option[Seq[TestFilterPlus]]  = None
    override def NOT: Option[TestFilterPlus]      = None
  }
  // one more field and unordered, should not compile
  case class TestFilterPlusUnordered(
      field1: Option[IntFilter],
      field5: Option[StringFilter],
      field4: Option[TimestampFilterOption],
      field2: Option[StringFilterOption],
      field3: Option[LongFilter]
  ) extends EntityFilter[TestFilterPlusUnordered] {
    override def AND: Option[Seq[TestFilterPlusUnordered]] = None
    override def OR: Option[Seq[TestFilterPlusUnordered]]  = None
    override def NOT: Option[TestFilterPlusUnordered]      = None
  }
  // one less field, should not compile
  case class TestFilterLess(
      field1: Option[IntFilter],
      field2: Option[StringFilterOption],
      field3: Option[LongFilter]
  ) extends EntityFilter[TestFilterLess] {
    override def AND: Option[Seq[TestFilterLess]] = None
    override def OR: Option[Seq[TestFilterLess]]  = None
    override def NOT: Option[TestFilterLess]      = None
  }
  // one less field and unordered, should not compile
  case class TestFilterLessUnordered(
      field2: Option[StringFilterOption],
      field1: Option[IntFilter],
      field3: Option[LongFilter]
  ) extends EntityFilter[TestFilterLessUnordered] {
    override def AND: Option[Seq[TestFilterLessUnordered]] = None
    override def OR: Option[Seq[TestFilterLessUnordered]]  = None
    override def NOT: Option[TestFilterLessUnordered]      = None
  }

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

//  "TableFilter typeclass" should "work correctly" in {
//    val tableFilter1: TableFilter[Test, TestFilter] =
//      TableFilter.derive[Test, TestFilter]()
//    val filter1 = TestFilter(
//      Some(IntFilter.empty),
//      Some(StringFilterOption.empty),
//      None,
//      None
//    )
//
//    tableFilter1.keys() shouldBe List('field1, 'field2, 'field3, 'field4)
//    tableFilter1.values(filter1) shouldBe List(filter1.field1, filter1.field2, filter1.field3, filter1.field4)
//
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
//  }
}
