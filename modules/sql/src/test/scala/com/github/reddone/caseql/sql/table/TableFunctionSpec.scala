package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.filter.models.{Filter, IntFilterOption, LongFilter, StringFilter}
import com.github.reddone.caseql.sql.filter.wrappers.{EntityFilter, RelationFilter}
import com.github.reddone.caseql.sql.modifier.models.{
  IntModifierOption,
  Modifier,
  ModifierAction,
  ModifierOptionAction,
  StringModifier
}
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import shapeless._
import shapeless.test.illTyped

class TableFunctionSpec extends AnyFlatSpec with Matchers {

  // generic wrapper
  trait Wrapper[T]
  case class TestGeneric(
      field1: String,
      field2: Wrapper[String],
      field3: Option[Int],
      field4: Wrapper[Option[Int]]
  )
  // filter with other fields
  case class TestFilter(
      field1: String,
      field2: Option[StringFilter],
      field3: Option[Int],
      field4: Option[IntFilterOption],
      AND: Option[Seq[TestFilter]],
      OR: Option[Seq[TestFilter]],
      NOT: Option[TestFilter]
  ) extends EntityFilter[TestFilter]
  // modifier with other fields
  case class TestModifier(
      field1: String,
      field2: Option[StringModifier],
      field3: Option[Int],
      field4: Option[IntModifierOption]
  ) extends EntityModifier[TestModifier]
  // relation filter with other fields
  case class TestRelationFilter(
      field: Long,
      field2: Option[LongFilter],
      field3: Option[RelationFilter[Unit, Unit, TestFilter]],
      AND: Option[Seq[TestRelationFilter]],
      OR: Option[Seq[TestRelationFilter]],
      NOT: Option[TestRelationFilter]
  ) extends EntityFilter[TestRelationFilter]

  "TableFunction extract" should "extract a generic type" in {
    object wrapperExtractor extends TableFunction.extract[Wrapper[_]]

    val generic1 = TestGeneric(
      "1",
      new Wrapper[String] {},
      Some(3),
      new Wrapper[Option[Int]] {}
    )

    type Correct = Wrapper[_] :: Wrapper[_] :: HNil
    type Wrong   = Wrapper[_] :: String :: HNil
    val result = LabelledGeneric[TestGeneric].to(generic1).flatMap(wrapperExtractor)

    """implicitly[<:<[result.type, Correct]]""" should compile
    """implicitly[<:<[result.type, Wrong]]""" shouldNot compile
    illTyped { """implicitly[<:<[result.type, Wrong]]""" }
  }

  "TableFunction extractFilter" should "extract Option[Filter[_]]" in {
    val filter1 = TestFilter(
      "1",
      Some(StringFilter.empty),
      Some(3),
      Some(IntFilterOption.empty),
      None,
      None,
      None
    )

    type Correct = Option[Filter[_]] :: Option[Filter[_]] :: HNil
    type Wrong   = Option[Filter[_]] :: String :: HNil

    val result = LabelledGeneric[TestFilter].to(filter1).flatMap(TableFunction.extractFilter)

    """implicitly[<:<[result.type, Correct]]""" should compile
    """implicitly[<:<[result.type, Wrong]]""" shouldNot compile
    illTyped { """implicitly[<:<[result.type, Wrong]]""" }
  }

  "TableFunction extractModifier" should "extract Option[Modifier[_]]" in {
    val modifier1 = TestModifier(
      "1",
      Some(StringModifier(ModifierAction.Default, None)),
      Some(3),
      Some(IntModifierOption(ModifierOptionAction.Default, None))
    )

    type Correct = Option[Modifier[_]] :: Option[Modifier[_]] :: HNil
    type Wrong   = Option[Modifier[_]] :: String :: HNil

    val result = LabelledGeneric[TestModifier].to(modifier1).flatMap(TableFunction.extractModifier)

    """implicitly[<:<[result.type, Correct]]""" should compile
    """implicitly[<:<[result.type, Wrong]]""" shouldNot compile
    illTyped { """implicitly[<:<[result.type, Wrong]]""" }
  }

  "TableFunction extractRelationFilter" should "extract Option[RelationFilter[_, _, _]]" in {
    val filter1 = TestFilter(
      "1",
      Some(StringFilter.empty),
      Some(3),
      Some(IntFilterOption.empty),
      None,
      None,
      None
    )
    val relationFilter1 = TestRelationFilter(
      1L,
      Some(LongFilter.empty),
      Some(RelationFilter[Unit, Unit, TestFilter](Some(filter1), None, None)),
      None,
      None,
      None
    )

    val result = LabelledGeneric[TestRelationFilter].to(relationFilter1).flatMap(TableFunction.extractRelationFilter)

    type Correct = Option[RelationFilter[_, _, _]] :: HNil
    type Wrong   = Option[RelationFilter[_, _, _]] :: Option[Filter[_]] :: HNil

    """implicitly[<:<[result.type, Correct]]""" should compile
    """implicitly[<:<[result.type, Wrong]]""" shouldNot compile
    illTyped { """implicitly[<:<[result.type, Wrong]]""" }
  }
}
