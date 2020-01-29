package com.github.reddone.caseql.sql.filter

import java.sql.{Date, Time, Timestamp}
import java.time.temporal.Temporal
import java.time._

import cats.implicits._
import com.github.reddone.caseql.sql.util.CirceDecoders._
import com.github.reddone.caseql.sql.util.FragmentUtils
import doobie._
import doobie.implicits._
import javasql._
import javatime._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

object models {

  trait Filter[+T] {
    def toOptionFragment(column: String): Option[Fragment]
  }

  type FilterOption[+T] = Filter[Option[T]]

  // AbstractNumeric

  abstract class AbstractNumericFilter[T: Numeric: Put](
      EQ: Option[T],
      NOT_EQ: Option[T],
      IN: Option[Seq[T]],
      NOT_IN: Option[Seq[T]],
      LT: Option[T],
      LTE: Option[T],
      GT: Option[T],
      GTE: Option[T]
  ) extends Filter[T] {
    override def toOptionFragment(column: String): Option[Fragment] = FragmentUtils.optionalAndOpt(
      ops.eq(column, EQ),
      ops.notEq(column, NOT_EQ),
      ops.in(column, IN),
      ops.notIn(column, NOT_IN),
      ops.lt(column, LT),
      ops.lte(column, LTE),
      ops.gt(column, GT),
      ops.gte(column, GTE)
    )
  }

  abstract class AbstractNumericFilterOption[T: Numeric: Put](
      EQ: Option[T],
      NOT_EQ: Option[T],
      IN: Option[Seq[T]],
      NOT_IN: Option[Seq[T]],
      LT: Option[T],
      LTE: Option[T],
      GT: Option[T],
      GTE: Option[T],
      IS_NULL: Option[Boolean]
  ) extends FilterOption[T] {
    override def toOptionFragment(column: String): Option[Fragment] = FragmentUtils.optionalAndOpt(
      ops.eq(column, EQ),
      ops.notEq(column, NOT_EQ),
      ops.in(column, IN),
      ops.notIn(column, NOT_IN),
      ops.lt(column, LT),
      ops.lte(column, LTE),
      ops.gt(column, GT),
      ops.gte(column, GTE),
      ops.isNull(column, IS_NULL)
    )
  }

  // AbstractTemporal

  abstract class AbstractTemporalFilter[T <: Temporal: Put](
      EQ: Option[T],
      NOT_EQ: Option[T],
      IN: Option[Seq[T]],
      NOT_IN: Option[Seq[T]],
      LT: Option[T],
      LTE: Option[T],
      GT: Option[T],
      GTE: Option[T]
  ) extends Filter[T] {
    override def toOptionFragment(column: String): Option[Fragment] = FragmentUtils.optionalAndOpt(
      ops.eq(column, EQ),
      ops.notEq(column, NOT_EQ),
      ops.in(column, IN),
      ops.notIn(column, NOT_IN),
      ops.lt(column, LT),
      ops.lte(column, LTE),
      ops.gt(column, GT),
      ops.gte(column, GTE)
    )
  }

  abstract class AbstractTemporalFilterOption[T <: Temporal: Put](
      EQ: Option[T],
      NOT_EQ: Option[T],
      IN: Option[Seq[T]],
      NOT_IN: Option[Seq[T]],
      LT: Option[T],
      LTE: Option[T],
      GT: Option[T],
      GTE: Option[T],
      IS_NULL: Option[Boolean]
  ) extends FilterOption[T] {
    override def toOptionFragment(column: String): Option[Fragment] = FragmentUtils.optionalAndOpt(
      ops.eq(column, EQ),
      ops.notEq(column, NOT_EQ),
      ops.in(column, IN),
      ops.notIn(column, NOT_IN),
      ops.lt(column, LT),
      ops.lte(column, LTE),
      ops.gt(column, GT),
      ops.gte(column, GTE),
      ops.isNull(column, IS_NULL)
    )
  }

  // AbstractDate

  abstract class AbstractDateFilter[T <: java.util.Date: Put](
      EQ: Option[T],
      NOT_EQ: Option[T],
      IN: Option[Seq[T]],
      NOT_IN: Option[Seq[T]],
      LT: Option[T],
      LTE: Option[T],
      GT: Option[T],
      GTE: Option[T]
  ) extends Filter[T] {
    override def toOptionFragment(column: String): Option[Fragment] = FragmentUtils.optionalAndOpt(
      ops.eq(column, EQ),
      ops.notEq(column, NOT_EQ),
      ops.in(column, IN),
      ops.notIn(column, NOT_IN),
      ops.lt(column, LT),
      ops.lte(column, LTE),
      ops.gt(column, GT),
      ops.gte(column, GTE)
    )
  }

  abstract class AbstractDateFilterOption[T <: java.util.Date: Put](
      EQ: Option[T],
      NOT_EQ: Option[T],
      IN: Option[Seq[T]],
      NOT_IN: Option[Seq[T]],
      LT: Option[T],
      LTE: Option[T],
      GT: Option[T],
      GTE: Option[T],
      IS_NULL: Option[Boolean]
  ) extends FilterOption[T] {
    override def toOptionFragment(column: String): Option[Fragment] = FragmentUtils.optionalAndOpt(
      ops.eq(column, EQ),
      ops.notEq(column, NOT_EQ),
      ops.in(column, IN),
      ops.notIn(column, NOT_IN),
      ops.lt(column, LT),
      ops.lte(column, LTE),
      ops.gt(column, GT),
      ops.gte(column, GTE),
      ops.isNull(column, IS_NULL)
    )
  }

  // Enumeration

  final case class EnumFilter[E <: Enumeration#Value: Put](
      EQ: Option[E],
      NOT_EQ: Option[E],
      IN: Option[Seq[E]],
      NOT_IN: Option[Seq[E]]
  ) extends Filter[E] {
    override def toOptionFragment(column: String): Option[Fragment] = FragmentUtils.optionalAndOpt(
      ops.eq(column, EQ),
      ops.notEq(column, NOT_EQ),
      ops.in(column, IN),
      ops.notIn(column, NOT_IN)
    )
  }

  object EnumFilter {
    // format: off
    def empty[E <: Enumeration#Value: Put]: EnumFilter[E] =
      EnumFilter[E](None, None, None, None)
    // format: on

    implicit def decoder[E <: Enumeration#Value: Decoder: Put]: Decoder[EnumFilter[E]] =
      deriveDecoder[EnumFilter[E]]
  }

  final case class EnumFilterOption[E <: Enumeration#Value: Put](
      EQ: Option[E],
      NOT_EQ: Option[E],
      IN: Option[Seq[E]],
      NOT_IN: Option[Seq[E]],
      IS_NULL: Option[Boolean]
  ) extends FilterOption[E] {
    override def toOptionFragment(column: String): Option[Fragment] = FragmentUtils.optionalAndOpt(
      ops.eq(column, EQ),
      ops.notEq(column, NOT_EQ),
      ops.in(column, IN),
      ops.notIn(column, NOT_IN),
      ops.isNull(column, IS_NULL)
    )
  }

  object EnumFilterOption {
    // format: off
    def empty[E <: Enumeration#Value: Decoder: Put]: EnumFilterOption[E] =
      EnumFilterOption[E](None, None, None, None, None)
    // format: on

    implicit def decoder[E <: Enumeration#Value: Decoder: Put]: Decoder[EnumFilterOption[E]] =
      deriveDecoder[EnumFilterOption[E]]
  }

  // Boolean

  final case class BooleanFilter(
      EQ: Option[Boolean]
  ) extends Filter[Boolean] {
    override def toOptionFragment(column: String): Option[Fragment] = FragmentUtils.optionalAndOpt(
      ops.eq(column, EQ)
    )
  }

  object BooleanFilter {
    // format: off
    val empty: BooleanFilter = BooleanFilter(None)
    // format: on

    implicit val decoder: Decoder[BooleanFilter] = deriveDecoder[BooleanFilter]
  }

  final case class BooleanFilterOption(
      EQ: Option[Boolean],
      IS_NULL: Option[Boolean]
  ) extends FilterOption[Boolean] {
    override def toOptionFragment(column: String): Option[Fragment] = FragmentUtils.optionalAndOpt(
      ops.eq(column, EQ),
      ops.isNull(column, IS_NULL)
    )
  }

  object BooleanFilterOption {
    // format: off
    val empty: BooleanFilterOption = BooleanFilterOption(None, None)
    // format: on

    implicit val decoder: Decoder[BooleanFilterOption] = deriveDecoder[BooleanFilterOption]
  }

  // Byte

  final case class ByteFilter(
      EQ: Option[Byte],
      NOT_EQ: Option[Byte]
  ) extends Filter[Byte] {
    override def toOptionFragment(column: String): Option[Fragment] = FragmentUtils.optionalAndOpt(
      ops.eq(column, EQ),
      ops.notEq(column, NOT_EQ)
    )
  }

  object ByteFilter {
    // format: off
    val empty: ByteFilter = ByteFilter(None, None)
    // format: on

    implicit val decoder: Decoder[ByteFilter] = deriveDecoder[ByteFilter]
  }

  final case class ByteFilterOption(
      EQ: Option[Byte],
      NOT_EQ: Option[Byte],
      IS_NULL: Option[Boolean]
  ) extends FilterOption[Byte] {
    override def toOptionFragment(column: String): Option[Fragment] = FragmentUtils.optionalAndOpt(
      ops.eq(column, EQ),
      ops.notEq(column, NOT_EQ),
      ops.isNull(column, IS_NULL)
    )
  }

  object ByteFilterOption {
    // format: off
    val empty: ByteFilterOption = ByteFilterOption(None, None, None)
    // format: on

    implicit val decoder: Decoder[ByteFilterOption] = deriveDecoder[ByteFilterOption]
  }

  // Array[Byte]

  final case class ByteArrayFilter(
      EQ: Option[Array[Byte]],
      NOT_EQ: Option[Array[Byte]]
  ) extends Filter[Array[Byte]] {
    override def toOptionFragment(column: String): Option[Fragment] = FragmentUtils.optionalAndOpt(
      ops.eq(column, EQ),
      ops.notEq(column, NOT_EQ)
    )
  }

  object ByteArrayFilter {
    // format: off
    val empty: ByteArrayFilter = ByteArrayFilter(None, None)
    // format: on

    implicit val decoder: Decoder[ByteArrayFilter] = deriveDecoder[ByteArrayFilter]
  }

  final case class ByteArrayFilterOption(
      EQ: Option[Array[Byte]],
      NOT_EQ: Option[Array[Byte]],
      IS_NULL: Option[Boolean]
  ) extends FilterOption[Array[Byte]] {
    override def toOptionFragment(column: String): Option[Fragment] = FragmentUtils.optionalAndOpt(
      ops.eq(column, EQ),
      ops.notEq(column, NOT_EQ),
      ops.isNull(column, IS_NULL)
    )
  }

  object ByteArrayFilterOption {
    // format: off
    val empty: ByteArrayFilterOption = ByteArrayFilterOption(None, None, None)
    // format: on

    implicit val decoder: Decoder[ByteArrayFilterOption] = deriveDecoder[ByteArrayFilterOption]
  }

  // Int

  final case class IntFilter(
      EQ: Option[Int],
      NOT_EQ: Option[Int],
      IN: Option[Seq[Int]],
      NOT_IN: Option[Seq[Int]],
      LT: Option[Int],
      LTE: Option[Int],
      GT: Option[Int],
      GTE: Option[Int]
  ) extends AbstractNumericFilter[Int](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE)

  object IntFilter {
    // format: off
    val empty: IntFilter = IntFilter(
      None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[IntFilter] = deriveDecoder[IntFilter]
  }

  final case class IntFilterOption(
      EQ: Option[Int],
      NOT_EQ: Option[Int],
      IN: Option[Seq[Int]],
      NOT_IN: Option[Seq[Int]],
      LT: Option[Int],
      LTE: Option[Int],
      GT: Option[Int],
      GTE: Option[Int],
      IS_NULL: Option[Boolean]
  ) extends AbstractNumericFilterOption[Int](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE, IS_NULL)

  object IntFilterOption {
    // format: off
    val empty: IntFilterOption = IntFilterOption(
      None, None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[IntFilterOption] = deriveDecoder[IntFilterOption]
  }

  // Long

  final case class LongFilter(
      EQ: Option[Long],
      NOT_EQ: Option[Long],
      IN: Option[Seq[Long]],
      NOT_IN: Option[Seq[Long]],
      LT: Option[Long],
      LTE: Option[Long],
      GT: Option[Long],
      GTE: Option[Long]
  ) extends AbstractNumericFilter[Long](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE)

  object LongFilter {
    // format: off
    val empty: LongFilter = LongFilter(
      None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[LongFilter] = deriveDecoder[LongFilter]
  }

  final case class LongFilterOption(
      EQ: Option[Long],
      NOT_EQ: Option[Long],
      IN: Option[Seq[Long]],
      NOT_IN: Option[Seq[Long]],
      LT: Option[Long],
      LTE: Option[Long],
      GT: Option[Long],
      GTE: Option[Long],
      IS_NULL: Option[Boolean]
  ) extends AbstractNumericFilterOption[Long](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE, IS_NULL)

  object LongFilterOption {
    // format: off
    val empty: LongFilterOption = LongFilterOption(
      None, None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[LongFilterOption] = deriveDecoder[LongFilterOption]
  }

  // Double

  final case class DoubleFilter(
      EQ: Option[Double],
      NOT_EQ: Option[Double],
      IN: Option[Seq[Double]],
      NOT_IN: Option[Seq[Double]],
      LT: Option[Double],
      LTE: Option[Double],
      GT: Option[Double],
      GTE: Option[Double]
  ) extends AbstractNumericFilter[Double](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE)

  object DoubleFilter {
    // format: off
    val empty: DoubleFilter = DoubleFilter(
      None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[DoubleFilter] = deriveDecoder[DoubleFilter]
  }

  final case class DoubleFilterOption(
      EQ: Option[Double],
      NOT_EQ: Option[Double],
      IN: Option[Seq[Double]],
      NOT_IN: Option[Seq[Double]],
      LT: Option[Double],
      LTE: Option[Double],
      GT: Option[Double],
      GTE: Option[Double],
      IS_NULL: Option[Boolean]
  ) extends AbstractNumericFilterOption[Double](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE, IS_NULL)

  object DoubleFilterOption {
    // format: off
    val empty: DoubleFilterOption = DoubleFilterOption(
      None, None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[DoubleFilterOption] = deriveDecoder[DoubleFilterOption]
  }

  // BigDecimal

  final case class BigDecimalFilter(
      EQ: Option[BigDecimal],
      NOT_EQ: Option[BigDecimal],
      IN: Option[Seq[BigDecimal]],
      NOT_IN: Option[Seq[BigDecimal]],
      LT: Option[BigDecimal],
      LTE: Option[BigDecimal],
      GT: Option[BigDecimal],
      GTE: Option[BigDecimal]
  ) extends AbstractNumericFilter[BigDecimal](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE)

  object BigDecimalFilter {
    // format: off
    val empty: BigDecimalFilter = BigDecimalFilter(
      None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[BigDecimalFilter] = deriveDecoder[BigDecimalFilter]
  }

  final case class BigDecimalFilterOption(
      EQ: Option[BigDecimal],
      NOT_EQ: Option[BigDecimal],
      IN: Option[Seq[BigDecimal]],
      NOT_IN: Option[Seq[BigDecimal]],
      LT: Option[BigDecimal],
      LTE: Option[BigDecimal],
      GT: Option[BigDecimal],
      GTE: Option[BigDecimal],
      IS_NULL: Option[Boolean]
  ) extends AbstractNumericFilterOption[BigDecimal](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE, IS_NULL)

  object BigDecimalFilterOption {
    // format: off
    val empty: BigDecimalFilterOption = BigDecimalFilterOption(
      None, None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[BigDecimalFilterOption] = deriveDecoder[BigDecimalFilterOption]
  }

  // String

  final case class StringFilter(
      EQ: Option[String],
      NOT_EQ: Option[String],
      IN: Option[Seq[String]],
      NOT_IN: Option[Seq[String]],
      CONTAINS: Option[String],
      CONTAINS_EVERY: Option[Seq[String]],
      CONTAINS_SOME: Option[Seq[String]],
      CONTAINS_NONE: Option[Seq[String]]
  ) extends Filter[String] {
    override def toOptionFragment(column: String): Option[Fragment] = FragmentUtils.optionalAndOpt(
      ops.eq(column, EQ),
      ops.notEq(column, NOT_EQ),
      ops.in(column, IN),
      ops.notIn(column, NOT_IN),
      ops.contains(column, CONTAINS),
      ops.containsEvery(column, CONTAINS_EVERY),
      ops.containsSome(column, CONTAINS_SOME),
      ops.containsNone(column, CONTAINS_NONE)
    )
  }

  object StringFilter {
    // format: off
    val empty: StringFilter = StringFilter(
      None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[StringFilter] = deriveDecoder[StringFilter]
  }

  final case class StringFilterOption(
      EQ: Option[String],
      NOT_EQ: Option[String],
      IN: Option[Seq[String]],
      NOT_IN: Option[Seq[String]],
      CONTAINS: Option[String],
      CONTAINS_EVERY: Option[Seq[String]],
      CONTAINS_SOME: Option[Seq[String]],
      CONTAINS_NONE: Option[Seq[String]],
      IS_NULL: Option[Boolean]
  ) extends FilterOption[String] {
    override def toOptionFragment(column: String): Option[Fragment] = FragmentUtils.optionalAndOpt(
      ops.eq(column, EQ),
      ops.notEq(column, NOT_EQ),
      ops.in(column, IN),
      ops.notIn(column, NOT_IN),
      ops.contains(column, CONTAINS),
      ops.containsEvery(column, CONTAINS_EVERY),
      ops.containsSome(column, CONTAINS_SOME),
      ops.containsNone(column, CONTAINS_NONE),
      ops.isNull(column, IS_NULL)
    )
  }

  object StringFilterOption {
    // format: off
    val empty: StringFilterOption = StringFilterOption(
      None, None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[StringFilterOption] = deriveDecoder[StringFilterOption]
  }

  // Instant

  final case class InstantFilter(
      EQ: Option[Instant],
      NOT_EQ: Option[Instant],
      IN: Option[Seq[Instant]],
      NOT_IN: Option[Seq[Instant]],
      LT: Option[Instant],
      LTE: Option[Instant],
      GT: Option[Instant],
      GTE: Option[Instant]
  ) extends AbstractTemporalFilter[Instant](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE)

  object InstantFilter {
    // format: off
    val empty: InstantFilter = InstantFilter(
      None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[InstantFilter] = deriveDecoder[InstantFilter]
  }

  final case class InstantFilterOption(
      EQ: Option[Instant],
      NOT_EQ: Option[Instant],
      IN: Option[Seq[Instant]],
      NOT_IN: Option[Seq[Instant]],
      LT: Option[Instant],
      LTE: Option[Instant],
      GT: Option[Instant],
      GTE: Option[Instant],
      IS_NULL: Option[Boolean]
  ) extends AbstractTemporalFilterOption[Instant](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE, IS_NULL)

  object InstantFilterOption {
    // format: off
    val empty: InstantFilterOption = InstantFilterOption(
      None, None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[InstantFilterOption] = deriveDecoder[InstantFilterOption]
  }

  // LocalDate

  final case class LocalDateFilter(
      EQ: Option[LocalDate],
      NOT_EQ: Option[LocalDate],
      IN: Option[Seq[LocalDate]],
      NOT_IN: Option[Seq[LocalDate]],
      LT: Option[LocalDate],
      LTE: Option[LocalDate],
      GT: Option[LocalDate],
      GTE: Option[LocalDate]
  ) extends AbstractTemporalFilter[LocalDate](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE)

  object LocalDateFilter {
    // format: off
    val empty: LocalDateFilter = LocalDateFilter(
      None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[LocalDateFilter] = deriveDecoder[LocalDateFilter]
  }

  final case class LocalDateFilterOption(
      EQ: Option[LocalDate],
      NOT_EQ: Option[LocalDate],
      IN: Option[Seq[LocalDate]],
      NOT_IN: Option[Seq[LocalDate]],
      LT: Option[LocalDate],
      LTE: Option[LocalDate],
      GT: Option[LocalDate],
      GTE: Option[LocalDate],
      IS_NULL: Option[Boolean]
  ) extends AbstractTemporalFilterOption[LocalDate](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE, IS_NULL)

  object LocalDateFilterOption {
    // format: off
    val empty: LocalDateFilterOption = LocalDateFilterOption(
      None, None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[LocalDateFilterOption] = deriveDecoder[LocalDateFilterOption]
  }

  // LocalTime

  final case class LocalTimeFilter(
      EQ: Option[LocalTime],
      NOT_EQ: Option[LocalTime],
      IN: Option[Seq[LocalTime]],
      NOT_IN: Option[Seq[LocalTime]],
      LT: Option[LocalTime],
      LTE: Option[LocalTime],
      GT: Option[LocalTime],
      GTE: Option[LocalTime]
  ) extends AbstractTemporalFilter[LocalTime](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE)

  object LocalTimeFilter {
    // format: off
    val empty: LocalTimeFilter = LocalTimeFilter(
      None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[LocalTimeFilter] = deriveDecoder[LocalTimeFilter]
  }

  final case class LocalTimeFilterOption(
      EQ: Option[LocalTime],
      NOT_EQ: Option[LocalTime],
      IN: Option[Seq[LocalTime]],
      NOT_IN: Option[Seq[LocalTime]],
      LT: Option[LocalTime],
      LTE: Option[LocalTime],
      GT: Option[LocalTime],
      GTE: Option[LocalTime],
      IS_NULL: Option[Boolean]
  ) extends AbstractTemporalFilterOption[LocalTime](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE, IS_NULL)

  object LocalTimeFilterOption {
    // format: off
    val empty: LocalTimeFilterOption = LocalTimeFilterOption(
      None, None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[LocalTimeFilterOption] = deriveDecoder[LocalTimeFilterOption]
  }

  // LocalDateTime

  final case class LocalDateTimeFilter(
      EQ: Option[LocalDateTime],
      NOT_EQ: Option[LocalDateTime],
      IN: Option[Seq[LocalDateTime]],
      NOT_IN: Option[Seq[LocalDateTime]],
      LT: Option[LocalDateTime],
      LTE: Option[LocalDateTime],
      GT: Option[LocalDateTime],
      GTE: Option[LocalDateTime]
  ) extends AbstractTemporalFilter[LocalDateTime](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE)

  object LocalDateTimeFilter {
    // format: off
    val empty: LocalDateTimeFilter = LocalDateTimeFilter(
      None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[LocalDateTimeFilter] = deriveDecoder[LocalDateTimeFilter]
  }

  final case class LocalDateTimeFilterOption(
      EQ: Option[LocalDateTime],
      NOT_EQ: Option[LocalDateTime],
      IN: Option[Seq[LocalDateTime]],
      NOT_IN: Option[Seq[LocalDateTime]],
      LT: Option[LocalDateTime],
      LTE: Option[LocalDateTime],
      GT: Option[LocalDateTime],
      GTE: Option[LocalDateTime],
      IS_NULL: Option[Boolean]
  ) extends AbstractTemporalFilterOption[LocalDateTime](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE, IS_NULL)

  object LocalDateTimeFilterOption {
    // format: off
    val empty: LocalDateTimeFilterOption = LocalDateTimeFilterOption(
      None, None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[LocalDateTimeFilterOption] = deriveDecoder[LocalDateTimeFilterOption]
  }

  // OffsetTime

  final case class OffsetTimeFilter(
      EQ: Option[OffsetTime],
      NOT_EQ: Option[OffsetTime],
      IN: Option[Seq[OffsetTime]],
      NOT_IN: Option[Seq[OffsetTime]],
      LT: Option[OffsetTime],
      LTE: Option[OffsetTime],
      GT: Option[OffsetTime],
      GTE: Option[OffsetTime]
  ) extends AbstractTemporalFilter[OffsetTime](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE)

  object OffsetTimeFilter {
    // format: off
    val empty: OffsetTimeFilter = OffsetTimeFilter(
      None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[OffsetTimeFilter] = deriveDecoder[OffsetTimeFilter]
  }

  final case class OffsetTimeFilterOption(
      EQ: Option[OffsetTime],
      NOT_EQ: Option[OffsetTime],
      IN: Option[Seq[OffsetTime]],
      NOT_IN: Option[Seq[OffsetTime]],
      LT: Option[OffsetTime],
      LTE: Option[OffsetTime],
      GT: Option[OffsetTime],
      GTE: Option[OffsetTime],
      IS_NULL: Option[Boolean]
  ) extends AbstractTemporalFilterOption[OffsetTime](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE, IS_NULL)

  object OffsetTimeFilterOption {
    // format: off
    val empty: OffsetTimeFilterOption = OffsetTimeFilterOption(
      None, None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[OffsetTimeFilterOption] = deriveDecoder[OffsetTimeFilterOption]
  }

  // OffsetDateTime

  final case class OffsetDateTimeFilter(
      EQ: Option[OffsetDateTime],
      NOT_EQ: Option[OffsetDateTime],
      IN: Option[Seq[OffsetDateTime]],
      NOT_IN: Option[Seq[OffsetDateTime]],
      LT: Option[OffsetDateTime],
      LTE: Option[OffsetDateTime],
      GT: Option[OffsetDateTime],
      GTE: Option[OffsetDateTime]
  ) extends AbstractTemporalFilter[OffsetDateTime](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE)

  object OffsetDateTimeFilter {
    // format: off
    val empty: OffsetDateTimeFilter = OffsetDateTimeFilter(
      None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[OffsetDateTimeFilter] = deriveDecoder[OffsetDateTimeFilter]
  }

  final case class OffsetDateTimeFilterOption(
      EQ: Option[OffsetDateTime],
      NOT_EQ: Option[OffsetDateTime],
      IN: Option[Seq[OffsetDateTime]],
      NOT_IN: Option[Seq[OffsetDateTime]],
      LT: Option[OffsetDateTime],
      LTE: Option[OffsetDateTime],
      GT: Option[OffsetDateTime],
      GTE: Option[OffsetDateTime],
      IS_NULL: Option[Boolean]
  ) extends AbstractTemporalFilterOption[OffsetDateTime](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE, IS_NULL)

  object OffsetDateTimeFilterOption {
    // format: off
    val empty: OffsetDateTimeFilterOption = OffsetDateTimeFilterOption(
      None, None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[OffsetDateTimeFilterOption] = deriveDecoder[OffsetDateTimeFilterOption]
  }

  // ZonedDateTime

  final case class ZonedDateTimeFilter(
      EQ: Option[ZonedDateTime],
      NOT_EQ: Option[ZonedDateTime],
      IN: Option[Seq[ZonedDateTime]],
      NOT_IN: Option[Seq[ZonedDateTime]],
      LT: Option[ZonedDateTime],
      LTE: Option[ZonedDateTime],
      GT: Option[ZonedDateTime],
      GTE: Option[ZonedDateTime]
  ) extends AbstractTemporalFilter[ZonedDateTime](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE)

  object ZonedDateTimeFilter {
    // format: off
    val empty: ZonedDateTimeFilter = ZonedDateTimeFilter(
      None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[ZonedDateTimeFilter] = deriveDecoder[ZonedDateTimeFilter]
  }

  final case class ZonedDateTimeFilterOption(
      EQ: Option[ZonedDateTime],
      NOT_EQ: Option[ZonedDateTime],
      IN: Option[Seq[ZonedDateTime]],
      NOT_IN: Option[Seq[ZonedDateTime]],
      LT: Option[ZonedDateTime],
      LTE: Option[ZonedDateTime],
      GT: Option[ZonedDateTime],
      GTE: Option[ZonedDateTime],
      IS_NULL: Option[Boolean]
  ) extends AbstractTemporalFilterOption[ZonedDateTime](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE, IS_NULL)

  object ZonedDateTimeFilterOption {
    // format: off
    val empty: ZonedDateTimeFilterOption = ZonedDateTimeFilterOption(
      None, None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[ZonedDateTimeFilterOption] = deriveDecoder[ZonedDateTimeFilterOption]
  }

  // Date

  final case class DateFilter(
      EQ: Option[Date],
      NOT_EQ: Option[Date],
      IN: Option[Seq[Date]],
      NOT_IN: Option[Seq[Date]],
      LT: Option[Date],
      LTE: Option[Date],
      GT: Option[Date],
      GTE: Option[Date]
  ) extends AbstractDateFilter[Date](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE)

  object DateFilter {
    // format: off
    val empty: DateFilter = DateFilter(
      None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[DateFilter] = deriveDecoder[DateFilter]
  }

  final case class DateFilterOption(
      EQ: Option[Date],
      NOT_EQ: Option[Date],
      IN: Option[Seq[Date]],
      NOT_IN: Option[Seq[Date]],
      LT: Option[Date],
      LTE: Option[Date],
      GT: Option[Date],
      GTE: Option[Date],
      IS_NULL: Option[Boolean]
  ) extends AbstractDateFilterOption[Date](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE, IS_NULL)

  object DateFilterOption {
    // format: off
    val empty: DateFilterOption = DateFilterOption(
      None, None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[DateFilterOption] = deriveDecoder[DateFilterOption]
  }

  // Time

  final case class TimeFilter(
      EQ: Option[Time],
      NOT_EQ: Option[Time],
      IN: Option[Seq[Time]],
      NOT_IN: Option[Seq[Time]],
      LT: Option[Time],
      LTE: Option[Time],
      GT: Option[Time],
      GTE: Option[Time]
  ) extends AbstractDateFilter[Time](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE)

  object TimeFilter {
    // format: off
    val empty: TimeFilter = TimeFilter(
      None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[TimeFilter] = deriveDecoder[TimeFilter]
  }

  final case class TimeFilterOption(
      EQ: Option[Time],
      NOT_EQ: Option[Time],
      IN: Option[Seq[Time]],
      NOT_IN: Option[Seq[Time]],
      LT: Option[Time],
      LTE: Option[Time],
      GT: Option[Time],
      GTE: Option[Time],
      IS_NULL: Option[Boolean]
  ) extends AbstractDateFilterOption[Time](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE, IS_NULL)

  object TimeFilterOption {
    // format: off
    val empty: TimeFilterOption = TimeFilterOption(
      None, None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[TimeFilterOption] = deriveDecoder[TimeFilterOption]
  }

  // Timestamp

  final case class TimestampFilter(
      EQ: Option[Timestamp],
      NOT_EQ: Option[Timestamp],
      IN: Option[Seq[Timestamp]],
      NOT_IN: Option[Seq[Timestamp]],
      LT: Option[Timestamp],
      LTE: Option[Timestamp],
      GT: Option[Timestamp],
      GTE: Option[Timestamp]
  ) extends AbstractDateFilter[Timestamp](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE)

  object TimestampFilter {
    // format: off
    val empty: TimestampFilter = TimestampFilter(
      None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[TimestampFilter] = deriveDecoder[TimestampFilter]
  }

  final case class TimestampFilterOption(
      EQ: Option[Timestamp],
      NOT_EQ: Option[Timestamp],
      IN: Option[Seq[Timestamp]],
      NOT_IN: Option[Seq[Timestamp]],
      LT: Option[Timestamp],
      LTE: Option[Timestamp],
      GT: Option[Timestamp],
      GTE: Option[Timestamp],
      IS_NULL: Option[Boolean]
  ) extends AbstractDateFilterOption[Timestamp](EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE, IS_NULL)

  object TimestampFilterOption {
    // format: off
    val empty: TimestampFilterOption = TimestampFilterOption(
      None, None, None, None, None, None, None, None, None
    )
    // format: on

    implicit val decoder: Decoder[TimestampFilterOption] = deriveDecoder[TimestampFilterOption]
  }
}
