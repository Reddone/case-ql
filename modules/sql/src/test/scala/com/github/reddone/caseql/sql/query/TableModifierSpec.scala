package com.github.reddone.caseql.sql.query

import java.sql.Timestamp

import com.github.reddone.caseql.sql.modifier.models._
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import shapeless.test.illTyped

class TableModifierSpec extends AnyFlatSpec with Matchers {

  // test model
  case class Test(
      field1: Int,
      field2: Option[String],
      field3: Long,
      field4: Option[Timestamp]
  )
  // simple case, should compile
  case class TestModifier(
      field1: Option[IntModifier],
      field2: Option[StringModifierOption],
      field3: Option[LongModifier],
      field4: Option[TimestampModifierOption]
  ) extends EntityModifier[TestModifier]
  // simple case but unordered, should compile
  case class TestModifierUnordered(
      field4: Option[TimestampModifierOption],
      field2: Option[StringModifierOption],
      field3: Option[LongModifier],
      field1: Option[IntModifier]
  ) extends EntityModifier[TestModifierUnordered]
  // with other fields, should compile
  case class TestModifierOther(
      field1: Option[IntModifier],
      field2: Option[StringModifierOption],
      field3: Option[LongModifier],
      field4: Option[TimestampModifierOption],
      otherField1: String,
      otherField2: Seq[Int]
  ) extends EntityModifier[TestModifierOther]
  // with other fields and unordered, should compile
  case class TestModifierOtherUnordered(
      otherField2: Seq[Int],
      field4: Option[TimestampModifierOption],
      field2: Option[StringModifierOption],
      field3: Option[LongModifier],
      otherField1: String,
      field1: Option[IntModifier]
  ) extends EntityModifier[TestModifierOtherUnordered]
  // one more field, should not compile
  case class TestModifierPlus(
      field1: Option[IntModifier],
      field2: Option[StringModifierOption],
      field3: Option[LongModifier],
      field4: Option[TimestampModifierOption],
      field5: Option[StringModifier]
  ) extends EntityModifier[TestModifierPlus]
  // one more field and unordered, should not compile
  case class TestModifierPlusUnordered(
      field1: Option[IntModifier],
      field5: Option[StringModifier],
      field4: Option[TimestampModifierOption],
      field2: Option[StringModifierOption],
      field3: Option[LongModifier]
  ) extends EntityModifier[TestModifierPlusUnordered]
  // one less field, should not compile
  case class TestModifierLess(
      field1: Option[IntModifier],
      field2: Option[StringModifierOption],
      field3: Option[LongModifier]
  ) extends EntityModifier[TestModifierLess]
  // one less field and unordered, should not compile
  case class TestModifierLessUnordered(
      field2: Option[StringModifierOption],
      field1: Option[IntModifier],
      field3: Option[LongModifier]
  ) extends EntityModifier[TestModifierLessUnordered]

  "TableModifier derivation" should "compile in the simple case" in {
    """TableModifier.derive[Test, TestModifier]()""" should compile
  }

  it should "compile in the unordered case" in {
    """TableModifier.derive[Test, TestModifierUnordered]()""" should compile
  }

  it should "compile in the other case" in {
    """TableModifier.derive[Test, TestModifierOther]()""" should compile
  }

  it should "compile in the other unordered case" in {
    """TableModifier.derive[Test, TestModifierOtherUnordered]()""" should compile
  }

  it should "not compile in the plus case" in {
    """TableModifier.derive[Test, TestModifierPlus]()""" shouldNot compile
    illTyped { """TableModifier.derive[Test, TestModifierPlus]()""" }
  }

  it should "not compile in the plus unordered case" in {
    """TableModifier.derive[Test, TestModifierPlusUnordered]()""" shouldNot compile
    illTyped { """TableModifier.derive[Test, TestModifierPlusUnordered]()""" }
  }

  it should "not compile in the less case" in {
    """TableModifier.derive[Test, TestModifierLess]()""" shouldNot compile
    illTyped { """TableModifier.derive[Test, TestModifierLess]()""" }
  }

  it should "not compile in the less unordered case" in {
    """TableModifier.derive[Test, TestModifierLessUnordered]()""" shouldNot compile
    illTyped { """TableModifier.derive[Test, TestModifierLessUnordered]()""" }
  }

//  "TableModifier typeclass" should "work correctly" in {
//    val tableModifier1: TableModifier[Test, TestModifier] =
//      TableModifier.derive[Test, TestModifier]()
//    val modifier1 = TestModifier(
//      Some(IntModifier(ModifierAction.Set, Some(1))),
//      Some(StringModifierOption(ModifierOptionAction.Null, None)),
//      None,
//      None
//    )
//
//    tableModifier1.keys() shouldBe List('field1, 'field2, 'field3, 'field4)
//    tableModifier1.values(modifier1) shouldBe List(
//      modifier1.field1,
//      modifier1.field2,
//      modifier1.field3,
//      modifier1.field4
//    )
//
//    val tableModifier2: TableModifier[Test, TestModifierUnordered] =
//      TableModifier.derive[Test, TestModifierUnordered]()
//    val modifier2 = TestModifierUnordered(
//      None,
//      Some(StringModifierOption(ModifierOptionAction.Null, None)),
//      None,
//      Some(IntModifier(ModifierAction.Set, Some(1)))
//    )
//
//    tableModifier2.keys() shouldBe List('field4, 'field2, 'field3, 'field1)
//    tableModifier2.values(modifier2) shouldBe List(
//      modifier2.field4,
//      modifier2.field2,
//      modifier2.field3,
//      modifier2.field1
//    )
//
//    val tableModifier3: TableModifier[Test, TestModifierOther] =
//      TableModifier.derive[Test, TestModifierOther]()
//    val modifier3 = TestModifierOther(
//      Some(IntModifier(ModifierAction.Set, Some(1))),
//      Some(StringModifierOption(ModifierOptionAction.Null, None)),
//      None,
//      None,
//      "5",
//      Seq(6)
//    )
//
//    tableModifier3.keys() shouldBe List('field1, 'field2, 'field3, 'field4)
//    tableModifier3.values(modifier3) shouldBe List(
//      modifier3.field1,
//      modifier3.field2,
//      modifier3.field3,
//      modifier3.field4
//    )
//
//    val tableModifier4: TableModifier[Test, TestModifierOtherUnordered] =
//      TableModifier.derive[Test, TestModifierOtherUnordered]()
//    val modifier4 = TestModifierOtherUnordered(
//      Seq(6),
//      None,
//      Some(StringModifierOption(ModifierOptionAction.Null, None)),
//      None,
//      "5",
//      Some(IntModifier(ModifierAction.Set, Some(1)))
//    )
//
//    tableModifier4.keys() shouldBe List('field4, 'field2, 'field3, 'field1)
//    tableModifier4.values(modifier4) shouldBe List(
//      modifier4.field4,
//      modifier4.field2,
//      modifier4.field3,
//      modifier4.field1
//    )
//  }
}
