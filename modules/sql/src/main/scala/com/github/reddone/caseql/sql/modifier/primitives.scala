package com.github.reddone.caseql.sql.modifier

import java.sql.{Date, Time, Timestamp}
import java.time._

import com.github.reddone.caseql.sql.tokens
import doobie._
import Fragment._
import doobie.implicits._
import javasql._
import javatime._

object primitives {

  object ModifierAction extends Enumeration {
    val Default: ModifierAction.Value = Value("DEFAULT")
    val Set: ModifierAction.Value     = Value("SET")
  }

  object ModifierOptionAction extends Enumeration {
    val Default: ModifierOptionAction.Value = Value("DEFAULT")
    val Set: ModifierOptionAction.Value     = Value("SET")
    val Null: ModifierOptionAction.Value    = Value("NULL")
  }

  trait Modifier[T] {
    def processPrimitiveModifier: Fragment
  }

  type ModifierOption[T] = Modifier[Option[T]]

  // Abstract

  abstract class AbstractGenericModifier[T: Put](
      action: ModifierAction.Value,
      value: Option[T]
  ) extends Modifier[T] {
    override def processPrimitiveModifier: Fragment = action match {
      case ModifierAction.Default =>
        const(tokens.Default)
      case ModifierAction.Set =>
        val v = value.getOrElse(throw new IllegalArgumentException("'value' field cannot be empty if you use SET"))
        fr"$v"
    }
  }

  abstract class AbstractGenericModifierOption[T: Put](
      action: ModifierOptionAction.Value,
      value: Option[T]
  ) extends ModifierOption[T] {
    override def processPrimitiveModifier: Fragment = action match {
      case ModifierOptionAction.Default =>
        const(tokens.Default)
      case ModifierOptionAction.Set =>
        val v = value.getOrElse(throw new IllegalArgumentException("'value' field cannot be empty if you use SET"))
        fr"$v"
      case ModifierOptionAction.Null =>
        val v: Option[T] = None
        fr"$v"
    }
  }

  // Enumeration

  final case class EnumModifier[E <: Enumeration#Value: Put](
      action: ModifierAction.Value,
      value: Option[E]
  ) extends AbstractGenericModifier[E](action, value)

  final case class EnumModifierOption[E <: Enumeration#Value: Put](
      action: ModifierOptionAction.Value,
      value: Option[E]
  ) extends AbstractGenericModifierOption[E](action, value)

  // Boolean

  final case class BooleanModifier(
      action: ModifierAction.Value,
      value: Option[Boolean]
  ) extends AbstractGenericModifier[Boolean](action, value)

  final case class BooleanModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[Boolean]
  ) extends AbstractGenericModifierOption[Boolean](action, value)

  // Byte

  final case class ByteModifier(
      action: ModifierAction.Value,
      value: Option[Byte]
  ) extends AbstractGenericModifier[Byte](action, value)

  final case class ByteModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[Byte]
  ) extends AbstractGenericModifierOption[Byte](action, value)

  // Array[Byte]

  final case class ByteArrayModifier(
      action: ModifierAction.Value,
      value: Option[Array[Byte]]
  ) extends AbstractGenericModifier[Array[Byte]](action, value)

  final case class ByteArrayModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[Array[Byte]]
  ) extends AbstractGenericModifierOption[Array[Byte]](action, value)

  // Int

  final case class IntModifier(
      action: ModifierAction.Value,
      value: Option[Int]
  ) extends AbstractGenericModifier[Int](action, value)

  final case class IntModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[Int]
  ) extends AbstractGenericModifierOption[Int](action, value)

  // Long

  final case class LongModifier(
      action: ModifierAction.Value,
      value: Option[Long]
  ) extends AbstractGenericModifier[Long](action, value)

  final case class LongModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[Long]
  ) extends AbstractGenericModifierOption[Long](action, value)

  // Double

  final case class DoubleModifier(
      action: ModifierAction.Value,
      value: Option[Double]
  ) extends AbstractGenericModifier[Double](action, value)

  final case class DoubleModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[Double]
  ) extends AbstractGenericModifierOption[Double](action, value)

  // BigDecimal

  final case class BigDecimalModifier(
      action: ModifierAction.Value,
      value: Option[BigDecimal]
  ) extends AbstractGenericModifier[BigDecimal](action, value)

  final case class BigDecimalModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[BigDecimal]
  ) extends AbstractGenericModifierOption[BigDecimal](action, value)

  // String

  final case class StringModifier(
      action: ModifierAction.Value,
      value: Option[String]
  ) extends AbstractGenericModifier[String](action, value)

  final case class StringModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[String]
  ) extends AbstractGenericModifierOption[String](action, value)

  // Instant

  final case class InstantModifier(
      action: ModifierAction.Value,
      value: Option[Instant]
  ) extends AbstractGenericModifier[Instant](action, value)

  final case class InstantModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[Instant]
  ) extends AbstractGenericModifierOption[Instant](action, value)

  // LocalDate

  final case class LocalDateModifier(
      action: ModifierAction.Value,
      value: Option[LocalDate]
  ) extends AbstractGenericModifier[LocalDate](action, value)

  final case class LocalDateModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[LocalDate]
  ) extends AbstractGenericModifierOption[LocalDate](action, value)

  // LocalTime

  final case class LocalTimeModifier(
      action: ModifierAction.Value,
      value: Option[LocalTime]
  ) extends AbstractGenericModifier[LocalTime](action, value)

  final case class LocalTimeModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[LocalTime]
  ) extends AbstractGenericModifierOption[LocalTime](action, value)

  // LocalDateTime

  final case class LocalDateTimeModifier(
      action: ModifierAction.Value,
      value: Option[LocalDateTime]
  ) extends AbstractGenericModifier[LocalDateTime](action, value)

  final case class LocalDateTimeModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[LocalDateTime]
  ) extends AbstractGenericModifierOption[LocalDateTime](action, value)

  // OffsetTime

  final case class OffsetTimeModifier(
      action: ModifierAction.Value,
      value: Option[OffsetTime]
  ) extends AbstractGenericModifier[OffsetTime](action, value)

  final case class OffsetTimeModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[OffsetTime]
  ) extends AbstractGenericModifierOption[OffsetTime](action, value)

  // OffsetDateTime

  final case class OffsetDateTimeModifier(
      action: ModifierAction.Value,
      value: Option[OffsetDateTime]
  ) extends AbstractGenericModifier[OffsetDateTime](action, value)

  final case class OffsetDateTimeModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[OffsetDateTime]
  ) extends AbstractGenericModifierOption[OffsetDateTime](action, value)

  // ZonedDateTime

  final case class ZonedDateTimeModifier(
      action: ModifierAction.Value,
      value: Option[ZonedDateTime]
  ) extends AbstractGenericModifier[ZonedDateTime](action, value)

  final case class ZonedDateTimeModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[ZonedDateTime]
  ) extends AbstractGenericModifierOption[ZonedDateTime](action, value)

  // Date

  final case class DateModifier(
      action: ModifierAction.Value,
      value: Option[Date]
  ) extends AbstractGenericModifier[Date](action, value)

  final case class DateModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[Date]
  ) extends AbstractGenericModifierOption[Date](action, value)

  // Time

  final case class TimeModifier(
      action: ModifierAction.Value,
      value: Option[Time]
  ) extends AbstractGenericModifier[Time](action, value)

  final case class TimeModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[Time]
  ) extends AbstractGenericModifierOption[Time](action, value)

  // Timestamp

  final case class TimestampModifier(
      action: ModifierAction.Value,
      value: Option[Timestamp]
  ) extends AbstractGenericModifier[Timestamp](action, value)

  final case class TimestampModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[Timestamp]
  ) extends AbstractGenericModifierOption[Timestamp](action, value)
}
