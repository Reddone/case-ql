package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.TestModel._
import com.github.reddone.caseql.sql.modifier.models._
import doobie._
import doobie.implicits._
import javasql._
import javatime._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import shapeless.test.illTyped

class TableModifierSpec extends AnyFlatSpec with Matchers {

  implicit val table: Table[Test, TestKey] = Table.derive[Test, TestKey]()
  val syntax: TableSyntax[Test]            = table.syntax

//  "TableModifier derivation" should "compile in the simple case" in {
//    """TableModifier.derive[Test, TestModifier]()""" should compile
//  }
//
//  it should "compile in the unordered case" in {
//    """TableModifier.derive[Test, TestModifierUnordered]()""" should compile
//  }
//
//  it should "compile in the other case" in {
//    """TableModifier.derive[Test, TestModifierOther]()""" should compile
//  }
//
//  it should "compile in the other unordered case" in {
//    """TableModifier.derive[Test, TestModifierOtherUnordered]()""" should compile
//  }
//
//  it should "not compile in the plus case" in {
//    """TableModifier.derive[Test, TestModifierPlus]()""" shouldNot compile
//    illTyped { """TableModifier.derive[Test, TestModifierPlus]()""" }
//  }
//
//  it should "not compile in the plus unordered case" in {
//    """TableModifier.derive[Test, TestModifierPlusUnordered]()""" shouldNot compile
//    illTyped { """TableModifier.derive[Test, TestModifierPlusUnordered]()""" }
//  }
//
//  it should "not compile in the less case" in {
//    """TableModifier.derive[Test, TestModifierLess]()""" shouldNot compile
//    illTyped { """TableModifier.derive[Test, TestModifierLess]()""" }
//  }
//
//  it should "not compile in the less unordered case" in {
//    """TableModifier.derive[Test, TestModifierLessUnordered]()""" shouldNot compile
//    illTyped { """TableModifier.derive[Test, TestModifierLessUnordered]()""" }
//  }

  "TableModifier typeclass" should "work correctly with EntityModifier[_]" in {
    val tableModifier1: TableModifier[Test, TestModifier] =
      TableModifier.derive[Test, TestModifier]()
    val modifier1 = TestModifier(
      Some(IntModifier(ModifierAction.Set, Some(1))),
      Some(StringModifierOption(ModifierOptionAction.Null, None)),
      None,
      None
    )
    val result1 = tableModifier1.primitiveModifierNamedFragments(modifier1)

    result1.map(_._1) shouldBe List(
      syntax.field1,
      syntax.field2,
      syntax.field3,
      syntax.field4
    )
    result1.map(_._2.map(_.toString)) shouldBe List(
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
    val result2 = tableModifier2.primitiveModifierNamedFragments(modifier2)

    result2.map(_._1) shouldBe List(
      syntax.field4,
      syntax.field2,
      syntax.field3,
      syntax.field1
    )
    result2.map(_._2.map(_.toString)) shouldBe List(
      None,
      Some("Fragment(\"? \")"),
      None,
      Some("Fragment(\"? \")")
    )
  }
}
