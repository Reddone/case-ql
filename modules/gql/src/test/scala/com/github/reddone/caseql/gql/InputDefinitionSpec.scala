package com.github.reddone.caseql.gql

import com.github.reddone.caseql.gql.InputDefinition._
import com.github.reddone.caseql.gql.TestInputDefinition._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sangria.ast
import sangria.macros._
import sangria.schema._
import sangria.validation.{QueryValidator, Violation}

class InputDefinitionSpec extends AnyFlatSpec with Matchers {

  val Query: ObjectType[Unit, Unit] = ObjectType[Unit, Unit](
    "Query",
    fields[Unit, Unit](
      Field(
        "liveness",
        StringType,
        Some("Liveness probe"),
        arguments = Nil,
        resolve = _ => "Hey there!"
      )
    )
  )

  val AllFilterTypes: List[Type with Named] = List(
    BooleanFilterType,
    BooleanFilterOptionType,
    ByteFilterType,
    ByteFilterOptionType,
    ByteArrayFilterType,
    ByteArrayFilterOptionType,
    IntFilterType,
    IntFilterOptionType,
    LongFilterType,
    LongFilterOptionType,
    DoubleFilterType,
    DoubleFilterOptionType,
    BigDecimalFilterType,
    BigDecimalFilterOptionType,
    StringFilterType,
    StringFilterOptionType,
    InstantFilterType,
    InstantFilterOptionType,
    LocalDateFilterType,
    LocalDateFilterOptionType,
    LocalTimeFilterType,
    LocalTimeFilterOptionType,
    LocalDateTimeFilterType,
    LocalDateTimeFilterOptionType,
    OffsetTimeFilterType,
    OffsetTimeFilterOptionType,
    OffsetDateTimeFilterType,
    OffsetDateTimeFilterOptionType,
    ZonedDateTimeFilterType,
    ZonedDateTimeFilterOptionType,
    DateFilterType,
    DateFilterOptionType,
    TimeFilterType,
    TimeFilterOptionType,
    TimestampFilterType,
    TimestampFilterOptionType
  )

  val AllModifierTypes: List[Type with Named] = List(
    BooleanModifierType,
    BooleanModifierOptionType,
    ByteModifierType,
    ByteModifierOptionType,
    ByteArrayModifierType,
    ByteArrayModifierOptionType,
    IntModifierType,
    IntModifierOptionType,
    LongModifierType,
    LongModifierOptionType,
    DoubleModifierType,
    DoubleModifierOptionType,
    BigDecimalModifierType,
    BigDecimalModifierOptionType,
    StringModifierType,
    StringModifierOptionType,
    InstantModifierType,
    InstantModifierOptionType,
    LocalDateModifierType,
    LocalDateModifierOptionType,
    LocalTimeModifierType,
    LocalTimeModifierOptionType,
    LocalDateTimeModifierType,
    LocalDateTimeModifierOptionType,
    OffsetTimeModifierType,
    OffsetTimeModifierOptionType,
    OffsetDateTimeModifierType,
    OffsetDateTimeModifierOptionType,
    ZonedDateTimeModifierType,
    ZonedDateTimeModifierOptionType,
    DateModifierType,
    DateModifierOptionType,
    TimeModifierType,
    TimeModifierOptionType,
    TimestampModifierType,
    TimestampModifierOptionType
  )

  val AllTestFilterTypes: List[Type with Named] = List(
    TestFilterType,
    TestLeftFilterType,
    TestDirectFilterType,
    TestRightFilterType,
    TestJunctionFilterType
  )

  val AllTestModifierTypes: List[Type with Named] = List(
    TestModifierType
  )

  val schema: Schema[Unit, Unit] = Schema(
    query = Query,
    mutation = None,
    subscription = None,
    additionalTypes = AllFilterTypes ++ AllModifierTypes ++ AllTestFilterTypes ++ AllTestModifierTypes
  )

  "InputDefinition" should "work for filters" in {
    val booleanInput      = gqlInpDoc"""
      {
        EQ: true
      }
      """
    val booleanViolations = validateInput(booleanInput, BooleanFilterType)

    booleanViolations shouldBe empty
  }

  it should "work for modifiers" in {

  }

  private def validateInput(doc: ast.InputDocument, inputType: InputType[_]): Vector[Violation] = {
    QueryValidator.default.validateInputDocument(schema, doc, inputType)
  }
}
