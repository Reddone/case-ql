package com.github.reddone.caseql.sql.table

import java.sql.Timestamp

import com.github.reddone.caseql.sql.modifier.models._
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import doobie._
import doobie.implicits._
import javasql._
import javatime._
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
  case class TestKey(
      field1: Int,
      field3: Long
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

  implicit val table: Table[Test, TestKey] = Table.derive[Test, TestKey]()

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

  "TableModifier typeclass" should "work correctly" in {
    val tableModifier1: TableModifier[Test, TestModifier] =
      TableModifier.derive[Test, TestModifier]()
    val modifier1 = TestModifier(
      Some(IntModifier(ModifierAction.Set, Some(1))),
      Some(StringModifierOption(ModifierOptionAction.Null, None)),
      None,
      None
    )
    val alias1  = "a1"
    val syntax1 = table.syntax.withAlias(Some(alias1))
    val result1 = tableModifier1.entityModifierNamedFragments(modifier1)

    result1(Some(alias1)).map(_._1) shouldBe List(
      syntax1.field1,
      syntax1.field2,
      syntax1.field3,
      syntax1.field4
    )
    result1(Some(alias1)).map(_._2.map(_.toString)) shouldBe List(
      Some("Fragment(\"? \")"),
      Some("Fragment(\"? \")"),
      None,
      None
    )

    val tableModifier2: TableModifier[Test, TestModifierUnordered] =
      TableModifier.derive[Test, TestModifierUnordered]()
    val modifier2 = TestModifierUnordered(
      None,
      Some(StringModifierOption(ModifierOptionAction.Null, None)),
      None,
      Some(IntModifier(ModifierAction.Set, Some(1)))
    )
    val alias2  = "a2"
    val syntax2 = table.syntax.withAlias(Some(alias2))
    val result2 = tableModifier2.entityModifierNamedFragments(modifier2)

    result2(Some(alias2)).map(_._1) shouldBe List(
      syntax2.field4,
      syntax2.field2,
      syntax2.field3,
      syntax2.field1
    )
    result2(Some(alias2)).map(_._2.map(_.toString)) shouldBe List(
      None,
      Some("Fragment(\"? \")"),
      None,
      Some("Fragment(\"? \")")
    )

    val tableModifier3: TableModifier[Test, TestModifierOther] =
      TableModifier.derive[Test, TestModifierOther]()
    val modifier3 = TestModifierOther(
      Some(IntModifier(ModifierAction.Set, Some(1))),
      Some(StringModifierOption(ModifierOptionAction.Null, None)),
      None,
      None,
      "5",
      Seq(6)
    )
    val alias3  = "a3"
    val syntax3 = table.syntax.withAlias(Some(alias3))
    val result3 = tableModifier3.entityModifierNamedFragments(modifier3)

    result3(Some(alias3)).map(_._1) shouldBe List(
      syntax3.field1,
      syntax3.field2,
      syntax3.field3,
      syntax3.field4
    )
    result3(Some(alias3)).map(_._2.map(_.toString)) shouldBe List(
      Some("Fragment(\"? \")"),
      Some("Fragment(\"? \")"),
      None,
      None
    )

    val tableModifier4: TableModifier[Test, TestModifierOtherUnordered] =
      TableModifier.derive[Test, TestModifierOtherUnordered]()
    val modifier4 = TestModifierOtherUnordered(
      Seq(6),
      None,
      Some(StringModifierOption(ModifierOptionAction.Null, None)),
      None,
      "5",
      Some(IntModifier(ModifierAction.Set, Some(1)))
    )
    val alias4  = "a4"
    val syntax4 = table.syntax.withAlias(Some(alias4))
    val result4 = tableModifier4.entityModifierNamedFragments(modifier4)

    result4(Some(alias4)).map(_._1) shouldBe List(
      syntax4.field4,
      syntax4.field2,
      syntax4.field3,
      syntax4.field1
    )
    result4(Some(alias2)).map(_._2.map(_.toString)) shouldBe List(
      None,
      Some("Fragment(\"? \")"),
      None,
      Some("Fragment(\"? \")")
    )
  }
}
