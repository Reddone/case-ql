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

  implicit val table: Table[Test, TestKey] = Table.derive[Test, TestKey](useTableAlias = false)
  val syntax: TableSyntax[Test]            = table.syntax

  "TableModifier derivation" should "compile in the simple case" in {
    """TableModifier.derive[Test, TestModifier]()""" should compile
  }

  it should "compile in the unordered case" in {
    """TableModifier.derive[Test, TestModifierUnordered]()""" should compile
  }

  it should "not compile in the other case" in {
    """TableModifier.derive[Test, TestModifierOther]()""" shouldNot compile
    illTyped { """TableModifier.derive[Test, TestModifierOther]()""" }
  }

  it should "not compile in the other unordered case" in {
    """TableModifier.derive[Test, TestModifierOtherUnordered]()""" shouldNot compile
    illTyped { """TableModifier.derive[Test, TestModifierOtherUnordered]()""" }
  }

  it should "not compile in the plus case" in {
    """TableModifier.derive[Test, TestModifierPlus]()""" shouldNot compile
    illTyped { """TableModifier.derive[Test, TestModifierPlus]()""" }
  }

  it should "not compile in the plus unordered case" in {
    """TableModifier.derive[Test, TestModifierPlusUnordered]()""" shouldNot compile
    illTyped { """TableModifier.derive[Test, TestModifierPlusUnordered]()""" }
  }

  it should "compile in the less case" in {
    """TableModifier.derive[Test, TestModifierLess]()""" should compile
  }

  it should "compile in the less unordered case" in {
    """TableModifier.derive[Test, TestModifierLessUnordered]()""" should compile
  }

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
      syntax.column("field1"),
      syntax.column("field2"),
      syntax.column("field3"),
      syntax.column("field4")
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
      syntax.column("field4"),
      syntax.column("field2"),
      syntax.column("field3"),
      syntax.column("field1")
    )
    result2.map(_._2.map(_.toString)) shouldBe List(
      None,
      Some("Fragment(\"? \")"),
      None,
      Some("Fragment(\"? \")")
    )
  }
}
