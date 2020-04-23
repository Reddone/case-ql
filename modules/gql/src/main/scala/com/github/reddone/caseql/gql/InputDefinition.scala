package com.github.reddone.caseql.gql

import java.time.temporal.Temporal

import com.github.reddone.caseql.sql.filter.wrappers.{EntityFilter, RelationFilter}
import com.github.reddone.caseql.sql.filter.models._
import com.github.reddone.caseql.sql.modifier.models._
import ByteTypeDefinition._
import EnumDefinition._
import JavaSqlTypeDefinition._
import JavaTimeTypeDefinition._
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
//    InputObjectType[IntFilter](
//      "IntFilter",
//      "Filter for an Int value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(IntType)),
//          InputField(NotEqName, OptionInputType(IntType)),
//          InputField(InName, OptionInputType(ListInputType(IntType))),
//          InputField(NotInName, OptionInputType(ListInputType(IntType))),
//          InputField(LtName, OptionInputType(IntType)),
//          InputField(LteName, OptionInputType(IntType)),
//          InputField(GtName, OptionInputType(IntType)),
//          InputField(GteName, OptionInputType(IntType))
//        )
//    )

  // FilterOption[Int]
  implicit val IntFilterOptionType: InputObjectType[IntFilterOption] =
    makeAbstractNumericFilterOptionType(
      "IntFilterOption",
      "Filter for an Option[Int] value",
      IntType
    )
//    InputObjectType[IntFilterOption](
//      "IntFilterOption",
//      "Filter for an Option[Int] value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(IntType)),
//          InputField(NotEqName, OptionInputType(IntType)),
//          InputField(InName, OptionInputType(ListInputType(IntType))),
//          InputField(NotInName, OptionInputType(ListInputType(IntType))),
//          InputField(LtName, OptionInputType(IntType)),
//          InputField(LteName, OptionInputType(IntType)),
//          InputField(GtName, OptionInputType(IntType)),
//          InputField(GteName, OptionInputType(IntType)),
//          InputField(IsNullName, OptionInputType(BooleanType))
//        )
//    )

  // Filter[Long]
  implicit val LongFilterType: InputObjectType[LongFilter] =
    makeAbstractNumericFilterType(
      "LongFilter",
      "Filter for a Long value",
      LongType
    )
//    InputObjectType[LongFilter](
//      "LongFilter",
//      "Filter for a Long value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(LongType)),
//          InputField(NotEqName, OptionInputType(LongType)),
//          InputField(InName, OptionInputType(ListInputType(LongType))),
//          InputField(NotInName, OptionInputType(ListInputType(LongType))),
//          InputField(LtName, OptionInputType(LongType)),
//          InputField(LteName, OptionInputType(LongType)),
//          InputField(GtName, OptionInputType(LongType)),
//          InputField(GteName, OptionInputType(LongType))
//        )
//    )

  // FilterOption[Long]
  implicit val LongFilterOptionType: InputObjectType[LongFilterOption] =
    makeAbstractNumericFilterOptionType(
      "LongFilterOption",
      "Filter for an Option[Long] value",
      LongType
    )
//    InputObjectType[LongFilterOption](
//      "LongFilterOption",
//      "Filter for an Option[Long] value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(LongType)),
//          InputField(NotEqName, OptionInputType(LongType)),
//          InputField(InName, OptionInputType(ListInputType(LongType))),
//          InputField(NotInName, OptionInputType(ListInputType(LongType))),
//          InputField(LtName, OptionInputType(LongType)),
//          InputField(LteName, OptionInputType(LongType)),
//          InputField(GtName, OptionInputType(LongType)),
//          InputField(GteName, OptionInputType(LongType)),
//          InputField(IsNullName, OptionInputType(BooleanType))
//        )
//    )

  // Filter[Double]
  implicit val DoubleFilterType: InputObjectType[DoubleFilter] =
    makeAbstractNumericFilterType(
      "DoubleFilter",
      "Filter for a Double value",
      FloatType
    )
//    InputObjectType[DoubleFilter](
//      "DoubleFilter",
//      "Filter for a Double value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(FloatType)),
//          InputField(NotEqName, OptionInputType(FloatType)),
//          InputField(InName, OptionInputType(ListInputType(FloatType))),
//          InputField(NotInName, OptionInputType(ListInputType(FloatType))),
//          InputField(LtName, OptionInputType(FloatType)),
//          InputField(LteName, OptionInputType(FloatType)),
//          InputField(GtName, OptionInputType(FloatType)),
//          InputField(GteName, OptionInputType(FloatType))
//        )
//    )

  // FilterOption[Double]
  implicit val DoubleFilterOptionType: InputObjectType[DoubleFilterOption] =
    makeAbstractNumericFilterOptionType(
      "DoubleFilterOption",
      "Filter for an Option[Double] value",
      FloatType
    )
//    InputObjectType[DoubleFilterOption](
//      "DoubleFilterOption",
//      "Filter for an Option[Double] value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(FloatType)),
//          InputField(NotEqName, OptionInputType(FloatType)),
//          InputField(InName, OptionInputType(ListInputType(FloatType))),
//          InputField(NotInName, OptionInputType(ListInputType(FloatType))),
//          InputField(LtName, OptionInputType(FloatType)),
//          InputField(LteName, OptionInputType(FloatType)),
//          InputField(GtName, OptionInputType(FloatType)),
//          InputField(GteName, OptionInputType(FloatType)),
//          InputField(IsNullName, OptionInputType(BooleanType))
//        )
//    )

  // Filter[BigDecimal]
  implicit val BigDecimalFilterType: InputObjectType[BigDecimalFilter] =
    makeAbstractNumericFilterType(
      "BigDecimalFilter",
      "Filter for a BigDecimal value",
      BigDecimalType
    )
//    InputObjectType[BigDecimalFilter](
//      "BigDecimalFilter",
//      "Filter for a BigDecimal value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(BigDecimalType)),
//          InputField(NotEqName, OptionInputType(BigDecimalType)),
//          InputField(InName, OptionInputType(ListInputType(BigDecimalType))),
//          InputField(NotInName, OptionInputType(ListInputType(BigDecimalType))),
//          InputField(LtName, OptionInputType(BigDecimalType)),
//          InputField(LteName, OptionInputType(BigDecimalType)),
//          InputField(GtName, OptionInputType(BigDecimalType)),
//          InputField(GteName, OptionInputType(BigDecimalType))
//        )
//    )

  // FilterOption[BigDecimal]
  implicit val BigDecimalFilterOptionType: InputObjectType[BigDecimalFilterOption] =
    makeAbstractNumericFilterOptionType(
      "BigDecimalFilterOption",
      "Filter for an Option[BigDecimal] value",
      BigDecimalType
    )
//  InputObjectType[BigDecimalFilterOption](
//      "BigDecimalFilterOption",
//      "Filter for an Option[BigDecimal] value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(BigDecimalType)),
//          InputField(NotEqName, OptionInputType(BigDecimalType)),
//          InputField(InName, OptionInputType(ListInputType(BigDecimalType))),
//          InputField(NotInName, OptionInputType(ListInputType(BigDecimalType))),
//          InputField(LtName, OptionInputType(BigDecimalType)),
//          InputField(LteName, OptionInputType(BigDecimalType)),
//          InputField(GtName, OptionInputType(BigDecimalType)),
//          InputField(GteName, OptionInputType(BigDecimalType)),
//          InputField(IsNullName, OptionInputType(BooleanType))
//        )
//    )

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
//    InputObjectType[InstantFilter](
//      "InstantFilter",
//      "Filter for a Instant value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(InstantType)),
//          InputField(NotEqName, OptionInputType(InstantType)),
//          InputField(InName, OptionInputType(ListInputType(InstantType))),
//          InputField(NotInName, OptionInputType(ListInputType(InstantType))),
//          InputField(LtName, OptionInputType(InstantType)),
//          InputField(LteName, OptionInputType(InstantType)),
//          InputField(GtName, OptionInputType(InstantType)),
//          InputField(GteName, OptionInputType(InstantType))
//        )
//    )

  // FilterOption[Instant]
  implicit val InstantFilterOptionType: InputObjectType[InstantFilterOption] =
    makeAbstractTemporalFilterOptionType(
      "InstantFilterOption",
      "Filter for an Option[Instant] value",
      InstantType
    )
//    InputObjectType[InstantFilterOption](
//      "InstantFilterOption",
//      "Filter for an Option[Instant] value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(InstantType)),
//          InputField(NotEqName, OptionInputType(InstantType)),
//          InputField(InName, OptionInputType(ListInputType(InstantType))),
//          InputField(NotInName, OptionInputType(ListInputType(InstantType))),
//          InputField(LtName, OptionInputType(InstantType)),
//          InputField(LteName, OptionInputType(InstantType)),
//          InputField(GtName, OptionInputType(InstantType)),
//          InputField(GteName, OptionInputType(InstantType)),
//          InputField(IsNullName, OptionInputType(BooleanType))
//        )
//    )

  // Filter[LocalDate]
  implicit val LocalDateFilterType: InputObjectType[LocalDateFilter] =
    makeAbstractTemporalFilterType(
      "LocalDateFilter",
      "Filter for a LocalDate value",
      LocalDateType
    )
//    InputObjectType[LocalDateFilter](
//      "LocalDateFilter",
//      "Filter for a LocalDate value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(LocalDateType)),
//          InputField(NotEqName, OptionInputType(LocalDateType)),
//          InputField(InName, OptionInputType(ListInputType(LocalDateType))),
//          InputField(NotInName, OptionInputType(ListInputType(LocalDateType))),
//          InputField(LtName, OptionInputType(LocalDateType)),
//          InputField(LteName, OptionInputType(LocalDateType)),
//          InputField(GtName, OptionInputType(LocalDateType)),
//          InputField(GteName, OptionInputType(LocalDateType))
//        )
//    )

  // FilterOption[LocalDate]
  implicit val LocalDateFilterOptionType: InputObjectType[LocalDateFilterOption] =
    makeAbstractTemporalFilterOptionType(
      "LocalDateFilterOption",
      "Filter for an Option[LocalDate] value",
      LocalDateType
    )
//    InputObjectType[LocalDateFilterOption](
//      "LocalDateFilterOption",
//      "Filter for an Option[LocalDate] value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(LocalDateType)),
//          InputField(NotEqName, OptionInputType(LocalDateType)),
//          InputField(InName, OptionInputType(ListInputType(LocalDateType))),
//          InputField(NotInName, OptionInputType(ListInputType(LocalDateType))),
//          InputField(LtName, OptionInputType(LocalDateType)),
//          InputField(LteName, OptionInputType(LocalDateType)),
//          InputField(GtName, OptionInputType(LocalDateType)),
//          InputField(GteName, OptionInputType(LocalDateType)),
//          InputField(IsNullName, OptionInputType(BooleanType))
//        )
//    )

  // Filter[LocalTime]
  implicit val LocalTimeFilterType: InputObjectType[LocalTimeFilter] =
    makeAbstractTemporalFilterType(
      "LocalTimeFilter",
      "Filter for a LocalTime value",
      LocalTimeType
    )
//    InputObjectType[LocalTimeFilter](
//      "LocalTimeFilter",
//      "Filter for a LocalTime value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(LocalTimeType)),
//          InputField(NotEqName, OptionInputType(LocalTimeType)),
//          InputField(InName, OptionInputType(ListInputType(LocalTimeType))),
//          InputField(NotInName, OptionInputType(ListInputType(LocalTimeType))),
//          InputField(LtName, OptionInputType(LocalTimeType)),
//          InputField(LteName, OptionInputType(LocalTimeType)),
//          InputField(GtName, OptionInputType(LocalTimeType)),
//          InputField(GteName, OptionInputType(LocalTimeType))
//        )
//    )

  // FilterOption[LocalTime]
  implicit val LocalTimeFilterOptionType: InputObjectType[LocalTimeFilterOption] =
    makeAbstractTemporalFilterOptionType(
      "LocalTimeFilterOption",
      "Filter for an Option[LocalTime] value",
      LocalTimeType
    )
//    InputObjectType[LocalTimeFilterOption](
//      "LocalTimeFilterOption",
//      "Filter for an Option[LocalTime] value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(LocalTimeType)),
//          InputField(NotEqName, OptionInputType(LocalTimeType)),
//          InputField(InName, OptionInputType(ListInputType(LocalTimeType))),
//          InputField(NotInName, OptionInputType(ListInputType(LocalTimeType))),
//          InputField(LtName, OptionInputType(LocalTimeType)),
//          InputField(LteName, OptionInputType(LocalTimeType)),
//          InputField(GtName, OptionInputType(LocalTimeType)),
//          InputField(GteName, OptionInputType(LocalTimeType)),
//          InputField(IsNullName, OptionInputType(BooleanType))
//        )
//    )

  // Filter[LocalDateTime]
  implicit val LocalDateTimeFilterType: InputObjectType[LocalDateTimeFilter] =
    makeAbstractTemporalFilterType(
      "LocalDateTimeFilter",
      "Filter for a LocalDateTime value",
      LocalDateTimeType
    )
//    InputObjectType[LocalDateTimeFilter](
//      "LocalDateTimeFilter",
//      "Filter for a LocalDateTime value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(LocalDateTimeType)),
//          InputField(NotEqName, OptionInputType(LocalDateTimeType)),
//          InputField(InName, OptionInputType(ListInputType(LocalDateTimeType))),
//          InputField(NotInName, OptionInputType(ListInputType(LocalDateTimeType))),
//          InputField(LtName, OptionInputType(LocalDateTimeType)),
//          InputField(LteName, OptionInputType(LocalDateTimeType)),
//          InputField(GtName, OptionInputType(LocalDateTimeType)),
//          InputField(GteName, OptionInputType(LocalDateTimeType))
//        )
//    )

  // FilterOption[LocalDateTime]
  implicit val LocalDateTimeFilterOptionType: InputObjectType[LocalDateTimeFilterOption] =
    makeAbstractTemporalFilterOptionType(
      "LocalDateTimeFilterOption",
      "Filter for an Option[LocalDateTime] value",
      LocalDateTimeType
    )
//    InputObjectType[LocalDateTimeFilterOption](
//      "LocalDateTimeFilterOption",
//      "Filter for an Option[LocalDateTime] value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(LocalDateTimeType)),
//          InputField(NotEqName, OptionInputType(LocalDateTimeType)),
//          InputField(InName, OptionInputType(ListInputType(LocalDateTimeType))),
//          InputField(NotInName, OptionInputType(ListInputType(LocalDateTimeType))),
//          InputField(LtName, OptionInputType(LocalDateTimeType)),
//          InputField(LteName, OptionInputType(LocalDateTimeType)),
//          InputField(GtName, OptionInputType(LocalDateTimeType)),
//          InputField(GteName, OptionInputType(LocalDateTimeType)),
//          InputField(IsNullName, OptionInputType(BooleanType))
//        )
//    )

  // Filter[OffsetTime]
  implicit val OffsetTimeFilterType: InputObjectType[OffsetTimeFilter] =
    makeAbstractTemporalFilterType(
      "OffsetTimeFilter",
      "Filter for a OffsetTime value",
      OffsetTimeType
    )
//    InputObjectType[OffsetTimeFilter](
//      "OffsetTimeFilter",
//      "Filter for a OffsetTime value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(OffsetTimeType)),
//          InputField(NotEqName, OptionInputType(OffsetTimeType)),
//          InputField(InName, OptionInputType(ListInputType(OffsetTimeType))),
//          InputField(NotInName, OptionInputType(ListInputType(OffsetTimeType))),
//          InputField(LtName, OptionInputType(OffsetTimeType)),
//          InputField(LteName, OptionInputType(OffsetTimeType)),
//          InputField(GtName, OptionInputType(OffsetTimeType)),
//          InputField(GteName, OptionInputType(OffsetTimeType))
//        )
//    )

  // FilterOption[OffsetTime]
  implicit val OffsetTimeFilterOptionType: InputObjectType[OffsetTimeFilterOption] =
    makeAbstractTemporalFilterOptionType(
      "OffsetTimeFilterOption",
      "Filter for an Option[OffsetTime] value",
      OffsetTimeType
    )
//    InputObjectType[OffsetTimeFilterOption](
//      "OffsetTimeFilterOption",
//      "Filter for an Option[OffsetTime] value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(OffsetTimeType)),
//          InputField(NotEqName, OptionInputType(OffsetTimeType)),
//          InputField(InName, OptionInputType(ListInputType(OffsetTimeType))),
//          InputField(NotInName, OptionInputType(ListInputType(OffsetTimeType))),
//          InputField(LtName, OptionInputType(OffsetTimeType)),
//          InputField(LteName, OptionInputType(OffsetTimeType)),
//          InputField(GtName, OptionInputType(OffsetTimeType)),
//          InputField(GteName, OptionInputType(OffsetTimeType)),
//          InputField(IsNullName, OptionInputType(BooleanType))
//        )
//    )

  // Filter[OffsetDateTime]
  implicit val OffsetDateTimeFilterType: InputObjectType[OffsetDateTimeFilter] =
    makeAbstractTemporalFilterType(
      "OffsetDateTimeFilter",
      "Filter for a OffsetDateTime value",
      OffsetDateTimeType
    )
//    InputObjectType[OffsetDateTimeFilter](
//      "OffsetDateTimeFilter",
//      "Filter for a OffsetDateTime value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(OffsetDateTimeType)),
//          InputField(NotEqName, OptionInputType(OffsetDateTimeType)),
//          InputField(InName, OptionInputType(ListInputType(OffsetDateTimeType))),
//          InputField(NotInName, OptionInputType(ListInputType(OffsetDateTimeType))),
//          InputField(LtName, OptionInputType(OffsetDateTimeType)),
//          InputField(LteName, OptionInputType(OffsetDateTimeType)),
//          InputField(GtName, OptionInputType(OffsetDateTimeType)),
//          InputField(GteName, OptionInputType(OffsetDateTimeType))
//        )
//    )

  // FilterOption[OffsetDateTime]
  implicit val OffsetDateTimeFilterOptionType: InputObjectType[OffsetDateTimeFilterOption] =
    makeAbstractTemporalFilterOptionType(
      "OffsetDateTimeFilterOption",
      "Filter for an Option[OffsetDateTime] value",
      OffsetDateTimeType
    )
//    InputObjectType[OffsetDateTimeFilterOption](
//      "OffsetDateTimeFilterOption",
//      "Filter for an Option[OffsetDateTime] value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(OffsetDateTimeType)),
//          InputField(NotEqName, OptionInputType(OffsetDateTimeType)),
//          InputField(InName, OptionInputType(ListInputType(OffsetDateTimeType))),
//          InputField(NotInName, OptionInputType(ListInputType(OffsetDateTimeType))),
//          InputField(LtName, OptionInputType(OffsetDateTimeType)),
//          InputField(LteName, OptionInputType(OffsetDateTimeType)),
//          InputField(GtName, OptionInputType(OffsetDateTimeType)),
//          InputField(GteName, OptionInputType(OffsetDateTimeType)),
//          InputField(IsNullName, OptionInputType(BooleanType))
//        )
//    )

  // Filter[Date]
  implicit val DateFilterType: InputObjectType[DateFilter] =
    makeAbstractDateFilterType(
      "DateFilter",
      "Filter for a Date value",
      DateType
    )
//    InputObjectType[DateFilter](
//      "DateFilter",
//      "Filter for a Date value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(DateType)),
//          InputField(NotEqName, OptionInputType(DateType)),
//          InputField(InName, OptionInputType(ListInputType(DateType))),
//          InputField(NotInName, OptionInputType(ListInputType(DateType))),
//          InputField(LtName, OptionInputType(DateType)),
//          InputField(LteName, OptionInputType(DateType)),
//          InputField(GtName, OptionInputType(DateType)),
//          InputField(GteName, OptionInputType(DateType))
//        )
//    )

  // FilterOption[Date]
  implicit val DateFilterOptionType: InputObjectType[DateFilterOption] =
    makeAbstractDateFilterOptionType(
      "DateFilterOption",
      "Filter for an Option[Date] value",
      DateType
    )
//    InputObjectType[DateFilterOption](
//      "DateFilterOption",
//      "Filter for an Option[Date] value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(DateType)),
//          InputField(NotEqName, OptionInputType(DateType)),
//          InputField(InName, OptionInputType(ListInputType(DateType))),
//          InputField(NotInName, OptionInputType(ListInputType(DateType))),
//          InputField(LtName, OptionInputType(DateType)),
//          InputField(LteName, OptionInputType(DateType)),
//          InputField(GtName, OptionInputType(DateType)),
//          InputField(GteName, OptionInputType(DateType)),
//          InputField(IsNullName, OptionInputType(BooleanType))
//        )
//    )

  // Filter[Time]
  implicit val TimeFilterType: InputObjectType[TimeFilter] =
    makeAbstractDateFilterType(
      "TimeFilter",
      "Filter for a Time value",
      TimeType
    )
//    InputObjectType[TimeFilter](
//      "TimeFilter",
//      "Filter for a Time value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(TimeType)),
//          InputField(NotEqName, OptionInputType(TimeType)),
//          InputField(InName, OptionInputType(ListInputType(TimeType))),
//          InputField(NotInName, OptionInputType(ListInputType(TimeType))),
//          InputField(LtName, OptionInputType(TimeType)),
//          InputField(LteName, OptionInputType(TimeType)),
//          InputField(GtName, OptionInputType(TimeType)),
//          InputField(GteName, OptionInputType(TimeType))
//        )
//    )

  // FilterOption[Time]
  implicit val TimeFilterOptionType: InputObjectType[TimeFilterOption] =
    makeAbstractDateFilterOptionType(
      "TimeFilterOption",
      "Filter for an Option[Time] value",
      TimeType
    )
//    InputObjectType[TimeFilterOption](
//      "TimeFilterOption",
//      "Filter for an Option[Time] value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(TimeType)),
//          InputField(NotEqName, OptionInputType(TimeType)),
//          InputField(InName, OptionInputType(ListInputType(TimeType))),
//          InputField(NotInName, OptionInputType(ListInputType(TimeType))),
//          InputField(LtName, OptionInputType(TimeType)),
//          InputField(LteName, OptionInputType(TimeType)),
//          InputField(GtName, OptionInputType(TimeType)),
//          InputField(GteName, OptionInputType(TimeType)),
//          InputField(IsNullName, OptionInputType(BooleanType))
//        )
//    )

  // Filter[Timestamp]
  implicit val TimestampFilterType: InputObjectType[TimestampFilter] =
    makeAbstractDateFilterType(
      "TimestampFilter",
      "Filter for a Timestamp value",
      TimestampType
    )
//    InputObjectType[TimestampFilter](
//      "TimestampFilter",
//      "Filter for a Timestamp value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(TimestampType)),
//          InputField(NotEqName, OptionInputType(TimestampType)),
//          InputField(InName, OptionInputType(ListInputType(TimestampType))),
//          InputField(NotInName, OptionInputType(ListInputType(TimestampType))),
//          InputField(LtName, OptionInputType(TimestampType)),
//          InputField(LteName, OptionInputType(TimestampType)),
//          InputField(GtName, OptionInputType(TimestampType)),
//          InputField(GteName, OptionInputType(TimestampType))
//        )
//    )

  // FilterOption[Timestamp]
  implicit val TimestampFilterOptionType: InputObjectType[TimestampFilterOption] =
    makeAbstractDateFilterOptionType(
      "TimestampFilterOption",
      "Filter for an Option[Timestamp] value",
      TimestampType
    )
//    InputObjectType[TimestampFilterOption](
//      "TimestampFilterOption",
//      "Filter for an Option[Timestamp] value",
//      () =>
//        List(
//          InputField(EqName, OptionInputType(TimestampType)),
//          InputField(NotEqName, OptionInputType(TimestampType)),
//          InputField(InName, OptionInputType(ListInputType(TimestampType))),
//          InputField(NotInName, OptionInputType(ListInputType(TimestampType))),
//          InputField(LtName, OptionInputType(TimestampType)),
//          InputField(LteName, OptionInputType(TimestampType)),
//          InputField(GtName, OptionInputType(TimestampType)),
//          InputField(GteName, OptionInputType(TimestampType)),
//          InputField(IsNullName, OptionInputType(BooleanType))
//        )
//    )

  // TODO: rewrite modifiers using these utilities

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
//    InputObjectType[BooleanModifier](
//      "BooleanModifier",
//      "Modifier for a Boolean value",
//      () =>
//        List(
//          InputField(ActionName, ModifierActionType),
//          InputField(ValueName, OptionInputType(BooleanType))
//        )
//    )

  // ModifierOption[Boolean]
  implicit val BooleanModifierOptionType: InputObjectType[BooleanModifierOption] =
    makeModifierOptionType(
      "BooleanModifierOption",
      "Modifier for an Option[Boolean] value",
      BooleanType
    )
//    InputObjectType[BooleanModifierOption](
//      "BooleanModifierOption",
//      "Modifier for an Option[Boolean] value",
//      () =>
//        List(
//          InputField(ActionName, ModifierOptionActionType),
//          InputField(ValueName, OptionInputType(BooleanType))
//        )
//    )

  // Modifier[Byte]
  implicit val ByteModifierType: InputObjectType[ByteModifier] =
    InputObjectType[ByteModifier](
      "ByteModifier",
      "Modifier for a Byte value",
      () =>
        List(
          InputField(ActionName, ModifierActionType),
          InputField(ValueName, OptionInputType(ByteType))
        )
    )

  // ModifierOption[Byte]
  implicit val ByteModifierOptionType: InputObjectType[ByteModifierOption] =
    InputObjectType[ByteModifierOption](
      "ByteModifierOption",
      "Modifier for an Option[Byte] value",
      () =>
        List(
          InputField(ActionName, ModifierOptionActionType),
          InputField(ValueName, OptionInputType(ByteType))
        )
    )

  // Modifier[Array[Byte]]
  implicit val ByteArrayModifierType: InputObjectType[ByteArrayModifier] =
    InputObjectType[ByteArrayModifier](
      "ByteArrayModifier",
      "Modifier for an Array[Byte] value",
      () =>
        List(
          InputField(ActionName, ModifierActionType),
          InputField(ValueName, OptionInputType(ByteArrayType))
        )
    )

  // ModifierOption[Array[Byte]]
  implicit val ByteArrayModifierOptionType: InputObjectType[ByteArrayModifierOption] =
    InputObjectType[ByteArrayModifierOption](
      "ByteArrayModifierOption",
      "Modifier for an Option[Array[Byte]] value",
      () =>
        List(
          InputField(ActionName, ModifierOptionActionType),
          InputField(ValueName, OptionInputType(ByteArrayType))
        )
    )

  // Modifier[Int]
  implicit val IntModifierType: InputObjectType[IntModifier] =
    InputObjectType[IntModifier](
      "IntModifier",
      "Modifier for a Int value",
      () =>
        List(
          InputField(ActionName, ModifierActionType),
          InputField(ValueName, OptionInputType(IntType))
        )
    )

  // ModifierOption[Int]
  implicit val IntModifierOptionType: InputObjectType[IntModifierOption] =
    InputObjectType[IntModifierOption](
      "IntModifierOption",
      "Modifier for an Option[Int] value",
      () =>
        List(
          InputField(ActionName, ModifierOptionActionType),
          InputField(ValueName, OptionInputType(IntType))
        )
    )

  // Modifier[Long]
  implicit val LongModifierType: InputObjectType[LongModifier] =
    InputObjectType[LongModifier](
      "LongModifier",
      "Modifier for a Long value",
      () =>
        List(
          InputField(ActionName, ModifierActionType),
          InputField(ValueName, OptionInputType(LongType))
        )
    )

  // ModifierOption[Long]
  implicit val LongModifierOptionType: InputObjectType[LongModifierOption] =
    InputObjectType[LongModifierOption](
      "LongModifierOption",
      "Modifier for an Option[Long] value",
      () =>
        List(
          InputField(ActionName, ModifierOptionActionType),
          InputField(ValueName, OptionInputType(LongType))
        )
    )

  // Modifier[Double]
  implicit val DoubleModifierType: InputObjectType[DoubleModifier] =
    InputObjectType[DoubleModifier](
      "DoubleModifier",
      "Modifier for a Double value",
      () =>
        List(
          InputField(ActionName, ModifierActionType),
          InputField(ValueName, OptionInputType(FloatType))
        )
    )

  // ModifierOption[Double]
  implicit val DoubleModifierOptionType: InputObjectType[DoubleModifierOption] =
    InputObjectType[DoubleModifierOption](
      "DoubleModifierOption",
      "Modifier for an Option[Double] value",
      () =>
        List(
          InputField(ActionName, ModifierOptionActionType),
          InputField(ValueName, OptionInputType(FloatType))
        )
    )

  // Modifier[BigDecimal]
  implicit val BigDecimalModifierType: InputObjectType[BigDecimalModifier] =
    InputObjectType[BigDecimalModifier](
      "BigDecimalModifier",
      "Modifier for a BigDecimal value",
      () =>
        List(
          InputField(ActionName, ModifierActionType),
          InputField(ValueName, OptionInputType(BigDecimalType))
        )
    )

  // ModifierOption[BigDecimal]
  implicit val BigDecimalModifierOptionType: InputObjectType[BigDecimalModifierOption] =
    InputObjectType[BigDecimalModifierOption](
      "BigDecimalModifierOption",
      "Modifier for an Option[BigDecimal] value",
      () =>
        List(
          InputField(ActionName, ModifierOptionActionType),
          InputField(ValueName, OptionInputType(BigDecimalType))
        )
    )

  // Modifier[String]
  implicit val StringModifierType: InputObjectType[StringModifier] =
    InputObjectType[StringModifier](
      "StringModifier",
      "Modifier for a String value",
      () =>
        List(
          InputField(ActionName, ModifierActionType),
          InputField(ValueName, OptionInputType(StringType))
        )
    )

  // ModifierOption[String]
  implicit val StringModifierOptionType: InputObjectType[StringModifierOption] =
    InputObjectType[StringModifierOption](
      "StringModifierOption",
      "Modifier for an Option[String] value",
      () =>
        List(
          InputField(ActionName, ModifierOptionActionType),
          InputField(ValueName, OptionInputType(StringType))
        )
    )

  // Modifier[Instant]
  implicit val InstantModifierType: InputObjectType[InstantModifier] =
    InputObjectType[InstantModifier](
      "InstantModifier",
      "Modifier for a Instant value",
      () =>
        List(
          InputField(ActionName, ModifierActionType),
          InputField(ValueName, OptionInputType(InstantType))
        )
    )

  // ModifierOption[Instant]
  implicit val InstantModifierOptionType: InputObjectType[InstantModifierOption] =
    InputObjectType[InstantModifierOption](
      "InstantModifierOption",
      "Modifier for an Option[Instant] value",
      () =>
        List(
          InputField(ActionName, ModifierOptionActionType),
          InputField(ValueName, OptionInputType(InstantType))
        )
    )

  // Modifier[LocalDate]
  implicit val LocalDateModifierType: InputObjectType[LocalDateModifier] =
    InputObjectType[LocalDateModifier](
      "LocalDateModifier",
      "Modifier for a LocalDate value",
      () =>
        List(
          InputField(ActionName, ModifierActionType),
          InputField(ValueName, OptionInputType(LocalDateType))
        )
    )

  // ModifierOption[LocalDate]
  implicit val LocalDateModifierOptionType: InputObjectType[LocalDateModifierOption] =
    InputObjectType[LocalDateModifierOption](
      "LocalDateModifierOption",
      "Modifier for an Option[LocalDate] value",
      () =>
        List(
          InputField(ActionName, ModifierOptionActionType),
          InputField(ValueName, OptionInputType(LocalDateType))
        )
    )

  // Modifier[LocalTime]
  implicit val LocalTimeModifierType: InputObjectType[LocalTimeModifier] =
    InputObjectType[LocalTimeModifier](
      "LocalTimeModifier",
      "Modifier for a LocalTime value",
      () =>
        List(
          InputField(ActionName, ModifierActionType),
          InputField(ValueName, OptionInputType(LocalTimeType))
        )
    )

  // ModifierOption[LocalTime]
  implicit val LocalTimeModifierOptionType: InputObjectType[LocalTimeModifierOption] =
    InputObjectType[LocalTimeModifierOption](
      "LocalTimeModifierOption",
      "Modifier for an Option[LocalTime] value",
      () =>
        List(
          InputField(ActionName, ModifierOptionActionType),
          InputField(ValueName, OptionInputType(LocalTimeType))
        )
    )

  // Modifier[LocalDateTime]
  implicit val LocalDateTimeModifierType: InputObjectType[LocalDateTimeModifier] =
    InputObjectType[LocalDateTimeModifier](
      "LocalDateTimeModifier",
      "Modifier for a LocalDateTime value",
      () =>
        List(
          InputField(ActionName, ModifierActionType),
          InputField(ValueName, OptionInputType(LocalDateTimeType))
        )
    )

  // ModifierOption[LocalDateTime]
  implicit val LocalDateTimeModifierOptionType: InputObjectType[LocalDateTimeModifierOption] =
    InputObjectType[LocalDateTimeModifierOption](
      "LocalDateTimeModifierOption",
      "Modifier for an Option[LocalDateTime] value",
      () =>
        List(
          InputField(ActionName, ModifierOptionActionType),
          InputField(ValueName, OptionInputType(LocalDateTimeType))
        )
    )

  // Modifier[OffsetTime]
  implicit val OffsetTimeModifierType: InputObjectType[OffsetTimeModifier] =
    InputObjectType[OffsetTimeModifier](
      "OffsetTimeModifier",
      "Modifier for a OffsetTime value",
      () =>
        List(
          InputField(ActionName, ModifierActionType),
          InputField(ValueName, OptionInputType(OffsetTimeType))
        )
    )

  // ModifierOption[OffsetTime]
  implicit val OffsetTimeModifierOptionType: InputObjectType[OffsetTimeModifierOption] =
    InputObjectType[OffsetTimeModifierOption](
      "OffsetTimeModifierOption",
      "Modifier for an Option[OffsetTime] value",
      () =>
        List(
          InputField(ActionName, ModifierOptionActionType),
          InputField(ValueName, OptionInputType(OffsetTimeType))
        )
    )

  // Modifier[OffsetDateTime]
  implicit val OffsetDateTimeModifierType: InputObjectType[OffsetDateTimeModifier] =
    InputObjectType[OffsetDateTimeModifier](
      "OffsetDateTimeModifier",
      "Modifier for a OffsetDateTime value",
      () =>
        List(
          InputField(ActionName, ModifierActionType),
          InputField(ValueName, OptionInputType(OffsetDateTimeType))
        )
    )

  // ModifierOption[OffsetDateTime]
  implicit val OffsetDateTimeModifierOptionType: InputObjectType[OffsetDateTimeModifierOption] =
    InputObjectType[OffsetDateTimeModifierOption](
      "OffsetDateTimeModifierOption",
      "Modifier for an Option[OffsetDateTime] value",
      () =>
        List(
          InputField(ActionName, ModifierOptionActionType),
          InputField(ValueName, OptionInputType(OffsetDateTimeType))
        )
    )

  // Modifier[ZonedDateTime]
  implicit val ZonedDateTimeModifierType: InputObjectType[ZonedDateTimeModifier] =
    InputObjectType[ZonedDateTimeModifier](
      "ZonedDateTimeModifier",
      "Modifier for a ZonedDateTime value",
      () =>
        List(
          InputField(ActionName, ModifierActionType),
          InputField(ValueName, OptionInputType(ZonedDateTimeType))
        )
    )

  // ModifierOption[ZonedDateTime]
  implicit val ZonedDateTimeModifierOptionType: InputObjectType[ZonedDateTimeModifierOption] =
    InputObjectType[ZonedDateTimeModifierOption](
      "ZonedDateTimeModifierOption",
      "Modifier for an Option[ZonedDateTime] value",
      () =>
        List(
          InputField(ActionName, ModifierOptionActionType),
          InputField(ValueName, OptionInputType(ZonedDateTimeType))
        )
    )

  // Modifier[Date]
  implicit val DateModifierType: InputObjectType[DateModifier] =
    InputObjectType[DateModifier](
      "DateModifier",
      "Modifier for a Date value",
      () =>
        List(
          InputField(ActionName, ModifierActionType),
          InputField(ValueName, OptionInputType(DateType))
        )
    )

  // ModifierOption[Date]
  implicit val DateModifierOptionType: InputObjectType[DateModifierOption] =
    InputObjectType[DateModifierOption](
      "DateModifierOption",
      "Modifier for an Option[Date] value",
      () =>
        List(
          InputField(ActionName, ModifierOptionActionType),
          InputField(ValueName, OptionInputType(DateType))
        )
    )

  // Modifier[Time]
  implicit val TimeModifierType: InputObjectType[TimeModifier] =
    InputObjectType[TimeModifier](
      "TimeModifier",
      "Modifier for a Time value",
      () =>
        List(
          InputField(ActionName, ModifierActionType),
          InputField(ValueName, OptionInputType(TimeType))
        )
    )

  // ModifierOption[Time]
  implicit val TimeModifierOptionType: InputObjectType[TimeModifierOption] =
    InputObjectType[TimeModifierOption](
      "TimeModifierOption",
      "Modifier for an Option[Time] value",
      () =>
        List(
          InputField(ActionName, ModifierOptionActionType),
          InputField(ValueName, OptionInputType(TimeType))
        )
    )

  // Modifier[Timestamp]
  implicit val TimestampModifierType: InputObjectType[TimestampModifier] =
    InputObjectType[TimestampModifier](
      "TimestampModifier",
      "Modifier for a Timestamp value",
      () =>
        List(
          InputField(ActionName, ModifierActionType),
          InputField(ValueName, OptionInputType(TimestampType))
        )
    )

  // ModifierOption[Timestamp]
  implicit val TimestampModifierOptionType: InputObjectType[TimestampModifierOption] =
    InputObjectType[TimestampModifierOption](
      "TimestampModifierOption",
      "Modifier for an Option[Timestamp] value",
      () =>
        List(
          InputField(ActionName, ModifierOptionActionType),
          InputField(ValueName, OptionInputType(TimestampType))
        )
    )

  implicit def relationFilterInputType[A, B, FB <: EntityFilter[FB]: TypeTag](
      implicit inputType: InputObjectType[FB]
  ): InputObjectType[RelationFilter[A, B, FB]] =
    InputObjectType[RelationFilter[A, B, FB]](
      s"RelationFilter for ${typeOf[FB].typeSymbol.name.toString}",
      s"RelationFilter for a ${typeOf[FB].typeSymbol.name.toString} entity filter",
      () =>
        List(
          InputField(EveryName, OptionInputType(inputType)),
          InputField(SomeName, OptionInputType(inputType)),
          InputField(NoneName, OptionInputType(inputType))
        )
    )
}
