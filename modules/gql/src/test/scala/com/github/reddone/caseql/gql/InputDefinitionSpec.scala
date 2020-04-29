package com.github.reddone.caseql.gql

import com.github.reddone.caseql.gql.EnumDefinition._
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
    ModifierActionType,
    ModifierOptionActionType,
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
    val testFilterInput      = gqlInpDoc"""
      {
        field1: {
          EQ: 1
          NOT_EQ: 1
          IN: [1, 2, 3]
          NOT_IN: [1, 2, 3]
          LT: 1
          LTE: 1
          GT: 1
          GTE: 1
        }
        field2: {
          EQ: "a"
          NOT_EQ: "a"
          IN: ["a", "b", "c"]
          NOT_IN: ["a", "b", "c"]
          CONTAINS: "a"
          CONTAINS_EVERY: ["a", "b", "c"]
          CONTAINS_SOME: ["a", "b", "c"]
          CONTAINS_NONE: ["a", "b", "c"]
          IS_NULL: false
        }
        field3: {
          EQ: 1
          NOT_EQ: 1
          IN: [1, 2, 3]
          NOT_IN: [1, 2, 3]
          LT: 1
          LTE: 1
          GT: 1
          GTE: 1
        }
        field4: {
          EQ: "2020-02-02 20:20:20"
          NOT_EQ: "2020-02-02 20:20:20"
          IN: ["2020-02-02 20:20:20", "2010-10-10 10:10:10"]
          NOT_IN: ["2020-02-02 20:20:20", "2010-10-10 10:10:10"]
          LT: "2020-02-02 20:20:20"
          LTE: "2020-02-02 20:20:20"
          GT: "2020-02-02 20:20:20"
          GTE: "2020-02-02 20:20:20"
          IS_NULL: false
        }
        AND: [
          {
            field1: {
              EQ: 1
            }
          }
        ]
        OR: [
          {
            field1: {
              EQ: 1
            }
          }
        ]
        NOT: {
          field1: {
            EQ: 1
          }
        }
      }
      """
    val testFilterViolations = validateInput(testFilterInput, TestFilterType)

    testFilterViolations shouldBe empty
  }

  it should "work for modifiers" in {
    val testModifierInput      = gqlInpDoc"""
      {
        field1: { 
          action: SET
          value: 1
        }
        field2: { 
          action: DEFAULT
        }
        field3: { 
          action: SET
          value: 1
        }
        field4: { 
          action: NULL
        }
      }
      """
    val testModifierViolations = validateInput(testModifierInput, TestModifierType)

    testModifierViolations shouldBe empty
  }

  private def validateInput(doc: ast.InputDocument, inputType: InputType[_]): Vector[Violation] = {
    QueryValidator.default.validateInputDocument(schema, doc, inputType)
  }
}
