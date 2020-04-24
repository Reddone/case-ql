package com.github.reddone.caseql.gql

import java.time.temporal.Temporal

import com.github.reddone.caseql.gql.ByteTypeDefinition._
import com.github.reddone.caseql.gql.EnumDefinition._
import com.github.reddone.caseql.gql.JavaSqlTypeDefinition._
import com.github.reddone.caseql.gql.JavaTimeTypeDefinition._
import com.github.reddone.caseql.sql.filter.models._
import com.github.reddone.caseql.sql.filter.wrappers.{EntityFilter, RelationFilter}
import com.github.reddone.caseql.sql.modifier.models._
import sangria.schema._

import scala.reflect.runtime.universe.{Symbol => _, _}

object InputDefinition {

  // filter field names
  private val EqName            = "EQ"
  private val NotEqName         = "NOT_EQ"
  private val InName            = "IN"
  private val NotInName         = "NOT_IN"
  private val LtName            = "LT"
  private val LteName           = "LTE"
  private val GtName            = "GT"
  private val GteName           = "GTE"
  private val ContainsName      = "CONTAINS"
  private val ContainsEveryName = "CONTAINS_EVERY"
  private val ContainsSomeName  = "CONTAINS_SOME"
  private val ContainsNoneName  = "CONTAINS_NONE"
  private val IsNullName        = "IS_NULL"
  private val EveryName         = "EVERY"
  private val SomeName          = "SOME"
  private val NoneName          = "NONE"
  // modifier field names
  private val ActionName = "action"
  private val ValueName  = "value"

  // FILTERS

  // Filter[T: Numeric]
  def makeAbstractNumericFilterType[T: Numeric, R <: AbstractNumericFilter[T]](
      name: String,
      description: String,
      baseType: ScalarType[T]
  ): InputObjectType[R] = InputObjectType[R](
    name,
    description,
    () =>
      List(
        InputField(EqName, OptionInputType(baseType)),
        InputField(NotEqName, OptionInputType(baseType)),
        InputField(InName, OptionInputType(ListInputType(baseType))),
        InputField(NotInName, OptionInputType(ListInputType(baseType))),
        InputField(LtName, OptionInputType(baseType)),
        InputField(LteName, OptionInputType(baseType)),
        InputField(GtName, OptionInputType(baseType)),
        InputField(GteName, OptionInputType(baseType))
      )
  )

  // FilterOption[T: Numeric]
  def makeAbstractNumericFilterOptionType[T: Numeric, R <: AbstractNumericFilterOption[T]](
      name: String,
      description: String,
      baseType: ScalarType[T]
  ): InputObjectType[R] = InputObjectType[R](
    name,
    description,
    () =>
      List(
        InputField(EqName, OptionInputType(baseType)),
        InputField(NotEqName, OptionInputType(baseType)),
        InputField(InName, OptionInputType(ListInputType(baseType))),
        InputField(NotInName, OptionInputType(ListInputType(baseType))),
        InputField(LtName, OptionInputType(baseType)),
        InputField(LteName, OptionInputType(baseType)),
        InputField(GtName, OptionInputType(baseType)),
        InputField(GteName, OptionInputType(baseType)),
        InputField(IsNullName, OptionInputType(BooleanType))
      )
  )

  // Filter[T <: Temporal]
  def makeAbstractTemporalFilterType[T <: Temporal, R <: AbstractTemporalFilter[T]](
      name: String,
      description: String,
      baseType: ScalarType[T]
  ): InputObjectType[R] = InputObjectType[R](
    name,
    description,
    () =>
      List(
        InputField(EqName, OptionInputType(baseType)),
        InputField(NotEqName, OptionInputType(baseType)),
        InputField(InName, OptionInputType(ListInputType(baseType))),
        InputField(NotInName, OptionInputType(ListInputType(baseType))),
        InputField(LtName, OptionInputType(baseType)),
        InputField(LteName, OptionInputType(baseType)),
        InputField(GtName, OptionInputType(baseType)),
        InputField(GteName, OptionInputType(baseType))
      )
  )

  // FilterOption[T <: Temporal]
  def makeAbstractTemporalFilterOptionType[T <: Temporal, R <: AbstractTemporalFilterOption[T]](
      name: String,
      description: String,
      baseType: ScalarType[T]
  ): InputObjectType[R] = InputObjectType[R](
    name,
    description,
    () =>
      List(
        InputField(EqName, OptionInputType(baseType)),
        InputField(NotEqName, OptionInputType(baseType)),
        InputField(InName, OptionInputType(ListInputType(baseType))),
        InputField(NotInName, OptionInputType(ListInputType(baseType))),
        InputField(LtName, OptionInputType(baseType)),
        InputField(LteName, OptionInputType(baseType)),
        InputField(GtName, OptionInputType(baseType)),
        InputField(GteName, OptionInputType(baseType)),
        InputField(IsNullName, OptionInputType(BooleanType))
      )
  )

  // Filter[T <: java.util.Date]
  def makeAbstractDateFilterType[T <: java.util.Date, R <: AbstractDateFilter[T]](
      name: String,
      description: String,
      baseType: ScalarType[T]
  ): InputObjectType[R] = InputObjectType[R](
    name,
    description,
    () =>
      List(
        InputField(EqName, OptionInputType(baseType)),
        InputField(NotEqName, OptionInputType(baseType)),
        InputField(InName, OptionInputType(ListInputType(baseType))),
        InputField(NotInName, OptionInputType(ListInputType(baseType))),
        InputField(LtName, OptionInputType(baseType)),
        InputField(LteName, OptionInputType(baseType)),
        InputField(GtName, OptionInputType(baseType)),
        InputField(GteName, OptionInputType(baseType))
      )
  )

  // FilterOption[T <: java.util.Date]
  def makeAbstractDateFilterOptionType[T <: java.util.Date, R <: AbstractDateFilterOption[T]](
      name: String,
      description: String,
      baseType: ScalarType[T]
  ): InputObjectType[R] = InputObjectType[R](
    name,
    description,
    () =>
      List(
        InputField(EqName, OptionInputType(baseType)),
        InputField(NotEqName, OptionInputType(baseType)),
        InputField(InName, OptionInputType(ListInputType(baseType))),
        InputField(NotInName, OptionInputType(ListInputType(baseType))),
        InputField(LtName, OptionInputType(baseType)),
        InputField(LteName, OptionInputType(baseType)),
        InputField(GtName, OptionInputType(baseType)),
        InputField(GteName, OptionInputType(baseType)),
        InputField(IsNullName, OptionInputType(BooleanType))
      )
  )

  // Filter[Boolean]
  implicit val BooleanFilterType: InputObjectType[BooleanFilter] =
    InputObjectType[BooleanFilter](
      "BooleanFilter",
      "Filter for a Boolean value",
      () =>
        List(
          InputField(EqName, OptionInputType(BooleanType))
        )
    )

  // FilterOption[Boolean]
  implicit val BooleanFilterOptionType: InputObjectType[BooleanFilterOption] =
    InputObjectType[BooleanFilterOption](
      "BooleanFilterOption",
      "Filter for an Option[Boolean] value",
      () =>
        List(
          InputField(EqName, OptionInputType(BooleanType)),
          InputField(IsNullName, OptionInputType(BooleanType))
        )
    )

  // Filter[Byte]
  implicit val ByteFilterType: InputObjectType[ByteFilter] =
    InputObjectType[ByteFilter](
      "ByteFilter",
      "Filter for a Byte value",
      () =>
        List(
          InputField(EqName, OptionInputType(ByteType)),
          InputField(NotEqName, OptionInputType(ByteType))
        )
    )

  // FilterOption[Byte]
  implicit val ByteFilterOptionType: InputObjectType[ByteFilterOption] =
    InputObjectType[ByteFilterOption](
      "ByteFilterOption",
      "Filter for an Option[Byte] value",
      () =>
        List(
          InputField(EqName, OptionInputType(ByteType)),
          InputField(NotEqName, OptionInputType(ByteType)),
          InputField(IsNullName, OptionInputType(BooleanType))
        )
    )

  // Filter[Array[Byte]]
  implicit val ByteArrayFilterType: InputObjectType[ByteArrayFilter] =
    InputObjectType[ByteArrayFilter](
      "ByteArrayFilter",
      "Filter for a Array[Byte] value",
      () =>
        List(
          InputField(EqName, OptionInputType(ByteArrayType)),
          InputField(NotEqName, OptionInputType(ByteArrayType))
        )
    )

  // FilterOption[Array[Byte]]
  implicit val ByteArrayFilterOptionType: InputObjectType[ByteArrayFilterOption] =
    InputObjectType[ByteArrayFilterOption](
      "ByteArrayFilterOption",
      "Filter for an Option[Array[Byte]] value",
      () =>
        List(
          InputField(EqName, OptionInputType(ByteArrayType)),
          InputField(NotEqName, OptionInputType(ByteArrayType)),
          InputField(IsNullName, OptionInputType(BooleanType))
        )
    )

  // Filter[Int]
  implicit val IntFilterType: InputObjectType[IntFilter] =
    makeAbstractNumericFilterType(
      "IntFilter",
      "Filter for an Int value",
      IntType
    )

  // FilterOption[Int]
  implicit val IntFilterOptionType: InputObjectType[IntFilterOption] =
    makeAbstractNumericFilterOptionType(
      "IntFilterOption",
      "Filter for an Option[Int] value",
      IntType
    )

  // Filter[Long]
  implicit val LongFilterType: InputObjectType[LongFilter] =
    makeAbstractNumericFilterType(
      "LongFilter",
      "Filter for a Long value",
      LongType
    )

  // FilterOption[Long]
  implicit val LongFilterOptionType: InputObjectType[LongFilterOption] =
    makeAbstractNumericFilterOptionType(
      "LongFilterOption",
      "Filter for an Option[Long] value",
      LongType
    )

  // Filter[Double]
  implicit val DoubleFilterType: InputObjectType[DoubleFilter] =
    makeAbstractNumericFilterType(
      "DoubleFilter",
      "Filter for a Double value",
      FloatType
    )

  // FilterOption[Double]
  implicit val DoubleFilterOptionType: InputObjectType[DoubleFilterOption] =
    makeAbstractNumericFilterOptionType(
      "DoubleFilterOption",
      "Filter for an Option[Double] value",
      FloatType
    )

  // Filter[BigDecimal]
  implicit val BigDecimalFilterType: InputObjectType[BigDecimalFilter] =
    makeAbstractNumericFilterType(
      "BigDecimalFilter",
      "Filter for a BigDecimal value",
      BigDecimalType
    )

  // FilterOption[BigDecimal]
  implicit val BigDecimalFilterOptionType: InputObjectType[BigDecimalFilterOption] =
    makeAbstractNumericFilterOptionType(
      "BigDecimalFilterOption",
      "Filter for an Option[BigDecimal] value",
      BigDecimalType
    )

  // Filter[String]
  implicit val StringFilterType: InputObjectType[StringFilter] =
    InputObjectType[StringFilter](
      "StringFilter",
      "Filter for a String value",
      () =>
        List(
          InputField(EqName, OptionInputType(StringType)),
          InputField(NotEqName, OptionInputType(StringType)),
          InputField(InName, OptionInputType(ListInputType(StringType))),
          InputField(NotInName, OptionInputType(ListInputType(StringType))),
          InputField(ContainsName, OptionInputType(StringType)),
          InputField(ContainsEveryName, OptionInputType(ListInputType(StringType))),
          InputField(ContainsSomeName, OptionInputType(ListInputType(StringType))),
          InputField(ContainsNoneName, OptionInputType(ListInputType(StringType)))
        )
    )

  // FilterOption[String]
  implicit val StringFilterOptionType: InputObjectType[StringFilterOption] =
    InputObjectType[StringFilterOption](
      "StringFilterOption",
      "Filter for an Option[String] value",
      () =>
        List(
          InputField(EqName, OptionInputType(StringType)),
          InputField(NotEqName, OptionInputType(StringType)),
          InputField(InName, OptionInputType(ListInputType(StringType))),
          InputField(NotInName, OptionInputType(ListInputType(StringType))),
          InputField(ContainsName, OptionInputType(StringType)),
          InputField(ContainsEveryName, OptionInputType(ListInputType(StringType))),
          InputField(ContainsSomeName, OptionInputType(ListInputType(StringType))),
          InputField(ContainsNoneName, OptionInputType(ListInputType(StringType))),
          InputField(IsNullName, OptionInputType(BooleanType))
        )
    )

  // Filter[Instant]
  implicit val InstantFilterType: InputObjectType[InstantFilter] =
    makeAbstractTemporalFilterType(
      "InstantFilter",
      "Filter for a Instant value",
      InstantType
    )

  // FilterOption[Instant]
  implicit val InstantFilterOptionType: InputObjectType[InstantFilterOption] =
    makeAbstractTemporalFilterOptionType(
      "InstantFilterOption",
      "Filter for an Option[Instant] value",
      InstantType
    )

  // Filter[LocalDate]
  implicit val LocalDateFilterType: InputObjectType[LocalDateFilter] =
    makeAbstractTemporalFilterType(
      "LocalDateFilter",
      "Filter for a LocalDate value",
      LocalDateType
    )

  // FilterOption[LocalDate]
  implicit val LocalDateFilterOptionType: InputObjectType[LocalDateFilterOption] =
    makeAbstractTemporalFilterOptionType(
      "LocalDateFilterOption",
      "Filter for an Option[LocalDate] value",
      LocalDateType
    )

  // Filter[LocalTime]
  implicit val LocalTimeFilterType: InputObjectType[LocalTimeFilter] =
    makeAbstractTemporalFilterType(
      "LocalTimeFilter",
      "Filter for a LocalTime value",
      LocalTimeType
    )

  // FilterOption[LocalTime]
  implicit val LocalTimeFilterOptionType: InputObjectType[LocalTimeFilterOption] =
    makeAbstractTemporalFilterOptionType(
      "LocalTimeFilterOption",
      "Filter for an Option[LocalTime] value",
      LocalTimeType
    )

  // Filter[LocalDateTime]
  implicit val LocalDateTimeFilterType: InputObjectType[LocalDateTimeFilter] =
    makeAbstractTemporalFilterType(
      "LocalDateTimeFilter",
      "Filter for a LocalDateTime value",
      LocalDateTimeType
    )

  // FilterOption[LocalDateTime]
  implicit val LocalDateTimeFilterOptionType: InputObjectType[LocalDateTimeFilterOption] =
    makeAbstractTemporalFilterOptionType(
      "LocalDateTimeFilterOption",
      "Filter for an Option[LocalDateTime] value",
      LocalDateTimeType
    )

  // Filter[OffsetTime]
  implicit val OffsetTimeFilterType: InputObjectType[OffsetTimeFilter] =
    makeAbstractTemporalFilterType(
      "OffsetTimeFilter",
      "Filter for a OffsetTime value",
      OffsetTimeType
    )

  // FilterOption[OffsetTime]
  implicit val OffsetTimeFilterOptionType: InputObjectType[OffsetTimeFilterOption] =
    makeAbstractTemporalFilterOptionType(
      "OffsetTimeFilterOption",
      "Filter for an Option[OffsetTime] value",
      OffsetTimeType
    )

  // Filter[OffsetDateTime]
  implicit val OffsetDateTimeFilterType: InputObjectType[OffsetDateTimeFilter] =
    makeAbstractTemporalFilterType(
      "OffsetDateTimeFilter",
      "Filter for a OffsetDateTime value",
      OffsetDateTimeType
    )

  // FilterOption[OffsetDateTime]
  implicit val OffsetDateTimeFilterOptionType: InputObjectType[OffsetDateTimeFilterOption] =
    makeAbstractTemporalFilterOptionType(
      "OffsetDateTimeFilterOption",
      "Filter for an Option[OffsetDateTime] value",
      OffsetDateTimeType
    )

  // Filter[Date]
  implicit val DateFilterType: InputObjectType[DateFilter] =
    makeAbstractDateFilterType(
      "DateFilter",
      "Filter for a Date value",
      DateType
    )

  // FilterOption[Date]
  implicit val DateFilterOptionType: InputObjectType[DateFilterOption] =
    makeAbstractDateFilterOptionType(
      "DateFilterOption",
      "Filter for an Option[Date] value",
      DateType
    )

  // Filter[Time]
  implicit val TimeFilterType: InputObjectType[TimeFilter] =
    makeAbstractDateFilterType(
      "TimeFilter",
      "Filter for a Time value",
      TimeType
    )

  // FilterOption[Time]
  implicit val TimeFilterOptionType: InputObjectType[TimeFilterOption] =
    makeAbstractDateFilterOptionType(
      "TimeFilterOption",
      "Filter for an Option[Time] value",
      TimeType
    )

  // Filter[Timestamp]
  implicit val TimestampFilterType: InputObjectType[TimestampFilter] =
    makeAbstractDateFilterType(
      "TimestampFilter",
      "Filter for a Timestamp value",
      TimestampType
    )

  // FilterOption[Timestamp]
  implicit val TimestampFilterOptionType: InputObjectType[TimestampFilterOption] =
    makeAbstractDateFilterOptionType(
      "TimestampFilterOption",
      "Filter for an Option[Timestamp] value",
      TimestampType
    )

  // MODIFIERS

  def makeModifierType[T, R <: AbstractGenericModifier[T]](
      name: String,
      description: String,
      baseType: ScalarType[T]
  ): InputObjectType[R] = InputObjectType[R](
    name,
    description,
    () =>
      List(
        InputField(ActionName, ModifierActionType),
        InputField(ValueName, OptionInputType(baseType))
      )
  )

  def makeModifierOptionType[T, R <: AbstractGenericModifierOption[T]](
      name: String,
      description: String,
      baseType: ScalarType[T]
  ): InputObjectType[R] = InputObjectType[R](
    name,
    description,
    () =>
      List(
        InputField(ActionName, ModifierOptionActionType),
        InputField(ValueName, OptionInputType(baseType))
      )
  )

  // Modifier[Boolean]
  implicit val BooleanModifierType: InputObjectType[BooleanModifier] =
    makeModifierType(
      "BooleanModifier",
      "Modifier for a Boolean value",
      BooleanType
    )

  // ModifierOption[Boolean]
  implicit val BooleanModifierOptionType: InputObjectType[BooleanModifierOption] =
    makeModifierOptionType(
      "BooleanModifierOption",
      "Modifier for an Option[Boolean] value",
      BooleanType
    )

  // Modifier[Byte]
  implicit val ByteModifierType: InputObjectType[ByteModifier] =
    makeModifierType(
      "ByteModifier",
      "Modifier for a Byte value",
      ByteType
    )

  // ModifierOption[Byte]
  implicit val ByteModifierOptionType: InputObjectType[ByteModifierOption] =
    makeModifierOptionType(
      "ByteModifierOption",
      "Modifier for an Option[Byte] value",
      ByteType
    )

  // Modifier[Array[Byte]]
  implicit val ByteArrayModifierType: InputObjectType[ByteArrayModifier] =
    makeModifierType(
      "ByteArrayModifier",
      "Modifier for an Array[Byte] value",
      ByteArrayType
    )

  // ModifierOption[Array[Byte]]
  implicit val ByteArrayModifierOptionType: InputObjectType[ByteArrayModifierOption] =
    makeModifierOptionType(
      "ByteArrayModifierOption",
      "Modifier for an Option[Array[Byte]] value",
      ByteArrayType
    )

  // Modifier[Int]
  implicit val IntModifierType: InputObjectType[IntModifier] =
    makeModifierType(
      "IntModifier",
      "Modifier for a Int value",
      IntType
    )

  // ModifierOption[Int]
  implicit val IntModifierOptionType: InputObjectType[IntModifierOption] =
    makeModifierOptionType(
      "IntModifierOption",
      "Modifier for an Option[Int] value",
      IntType
    )

  // Modifier[Long]
  implicit val LongModifierType: InputObjectType[LongModifier] =
    makeModifierType(
      "LongModifier",
      "Modifier for a Long value",
      LongType
    )

  // ModifierOption[Long]
  implicit val LongModifierOptionType: InputObjectType[LongModifierOption] =
    makeModifierOptionType(
      "LongModifierOption",
      "Modifier for an Option[Long] value",
      LongType
    )

  // Modifier[Double]
  implicit val DoubleModifierType: InputObjectType[DoubleModifier] =
    makeModifierType(
      "DoubleModifier",
      "Modifier for a Double value",
      FloatType
    )

  // ModifierOption[Double]
  implicit val DoubleModifierOptionType: InputObjectType[DoubleModifierOption] =
    makeModifierOptionType(
      "DoubleModifierOption",
      "Modifier for an Option[Double] value",
      FloatType
    )

  // Modifier[BigDecimal]
  implicit val BigDecimalModifierType: InputObjectType[BigDecimalModifier] =
    makeModifierType(
      "BigDecimalModifier",
      "Modifier for a BigDecimal value",
      BigDecimalType
    )

  // ModifierOption[BigDecimal]
  implicit val BigDecimalModifierOptionType: InputObjectType[BigDecimalModifierOption] =
    makeModifierOptionType(
      "BigDecimalModifierOption",
      "Modifier for an Option[BigDecimal] value",
      BigDecimalType
    )

  // Modifier[String]
  implicit val StringModifierType: InputObjectType[StringModifier] =
    makeModifierType(
      "StringModifier",
      "Modifier for a String value",
      StringType
    )

  // ModifierOption[String]
  implicit val StringModifierOptionType: InputObjectType[StringModifierOption] =
    makeModifierOptionType(
      "StringModifierOption",
      "Modifier for an Option[String] value",
      StringType
    )

  // Modifier[Instant]
  implicit val InstantModifierType: InputObjectType[InstantModifier] =
    makeModifierType(
      "InstantModifier",
      "Modifier for a Instant value",
      InstantType
    )

  // ModifierOption[Instant]
  implicit val InstantModifierOptionType: InputObjectType[InstantModifierOption] =
    makeModifierOptionType(
      "InstantModifierOption",
      "Modifier for an Option[Instant] value",
      InstantType
    )

  // Modifier[LocalDate]
  implicit val LocalDateModifierType: InputObjectType[LocalDateModifier] =
    makeModifierType(
      "LocalDateModifier",
      "Modifier for a LocalDate value",
      LocalDateType
    )

  // ModifierOption[LocalDate]
  implicit val LocalDateModifierOptionType: InputObjectType[LocalDateModifierOption] =
    makeModifierOptionType(
      "LocalDateModifierOption",
      "Modifier for an Option[LocalDate] value",
      LocalDateType
    )

  // Modifier[LocalTime]
  implicit val LocalTimeModifierType: InputObjectType[LocalTimeModifier] =
    makeModifierType(
      "LocalTimeModifier",
      "Modifier for a LocalTime value",
      LocalTimeType
    )

  // ModifierOption[LocalTime]
  implicit val LocalTimeModifierOptionType: InputObjectType[LocalTimeModifierOption] =
    makeModifierOptionType(
      "LocalTimeModifierOption",
      "Modifier for an Option[LocalTime] value",
      LocalTimeType
    )

  // Modifier[LocalDateTime]
  implicit val LocalDateTimeModifierType: InputObjectType[LocalDateTimeModifier] =
    makeModifierType(
      "LocalDateTimeModifier",
      "Modifier for a LocalDateTime value",
      LocalDateTimeType
    )

  // ModifierOption[LocalDateTime]
  implicit val LocalDateTimeModifierOptionType: InputObjectType[LocalDateTimeModifierOption] =
    makeModifierOptionType(
      "LocalDateTimeModifierOption",
      "Modifier for an Option[LocalDateTime] value",
      LocalDateTimeType
    )

  // Modifier[OffsetTime]
  implicit val OffsetTimeModifierType: InputObjectType[OffsetTimeModifier] =
    makeModifierType(
      "OffsetTimeModifier",
      "Modifier for a OffsetTime value",
      OffsetTimeType
    )

  // ModifierOption[OffsetTime]
  implicit val OffsetTimeModifierOptionType: InputObjectType[OffsetTimeModifierOption] =
    makeModifierOptionType(
      "OffsetTimeModifierOption",
      "Modifier for an Option[OffsetTime] value",
      OffsetTimeType
    )

  // Modifier[OffsetDateTime]
  implicit val OffsetDateTimeModifierType: InputObjectType[OffsetDateTimeModifier] =
    makeModifierType(
      "OffsetDateTimeModifier",
      "Modifier for a OffsetDateTime value",
      OffsetDateTimeType
    )

  // ModifierOption[OffsetDateTime]
  implicit val OffsetDateTimeModifierOptionType: InputObjectType[OffsetDateTimeModifierOption] =
    makeModifierOptionType(
      "OffsetDateTimeModifierOption",
      "Modifier for an Option[OffsetDateTime] value",
      OffsetDateTimeType
    )

  // Modifier[ZonedDateTime]
  implicit val ZonedDateTimeModifierType: InputObjectType[ZonedDateTimeModifier] =
    makeModifierType(
      "ZonedDateTimeModifier",
      "Modifier for a ZonedDateTime value",
      ZonedDateTimeType
    )

  // ModifierOption[ZonedDateTime]
  implicit val ZonedDateTimeModifierOptionType: InputObjectType[ZonedDateTimeModifierOption] =
    makeModifierOptionType(
      "ZonedDateTimeModifierOption",
      "Modifier for an Option[ZonedDateTime] value",
      ZonedDateTimeType
    )

  // Modifier[Date]
  implicit val DateModifierType: InputObjectType[DateModifier] =
    makeModifierType(
      "DateModifier",
      "Modifier for a Date value",
      DateType
    )

  // ModifierOption[Date]
  implicit val DateModifierOptionType: InputObjectType[DateModifierOption] =
    makeModifierOptionType(
      "DateModifierOption",
      "Modifier for an Option[Date] value",
      DateType
    )

  // Modifier[Time]
  implicit val TimeModifierType: InputObjectType[TimeModifier] =
    makeModifierType(
      "TimeModifier",
      "Modifier for a Time value",
      TimeType
    )

  // ModifierOption[Time]
  implicit val TimeModifierOptionType: InputObjectType[TimeModifierOption] =
    makeModifierOptionType(
      "TimeModifierOption",
      "Modifier for an Option[Time] value",
      TimeType
    )

  // Modifier[Timestamp]
  implicit val TimestampModifierType: InputObjectType[TimestampModifier] =
    makeModifierType(
      "TimestampModifier",
      "Modifier for a Timestamp value",
      TimestampType
    )

  // ModifierOption[Timestamp]
  implicit val TimestampModifierOptionType: InputObjectType[TimestampModifierOption] =
    makeModifierOptionType(
      "TimestampModifierOption",
      "Modifier for an Option[Timestamp] value",
      TimestampType
    )

  implicit def relationFilterType[A, B, FB <: EntityFilter[FB]: TypeTag](
      implicit inputType: InputObjectType[FB]
  ): InputObjectType[RelationFilter[A, B, FB]] =
    InputObjectType[RelationFilter[A, B, FB]](
      s"${typeOf[FB].typeSymbol.name.toString}Relation",
      s"Relation for ${typeOf[FB].typeSymbol.name.toString}",
      () =>
        List(
          InputField(EveryName, OptionInputType(inputType)),
          InputField(SomeName, OptionInputType(inputType)),
          InputField(NoneName, OptionInputType(inputType))
        )
    )
}
