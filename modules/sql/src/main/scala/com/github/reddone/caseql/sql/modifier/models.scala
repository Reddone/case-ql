package com.github.reddone.caseql.sql.modifier

import java.sql.{Date, Time, Timestamp}
import java.time._

import com.github.reddone.caseql.sql.tokens
import com.github.reddone.caseql.sql.util.CirceDecoders._
import doobie._
import doobie.implicits._
import Fragment._
import javasql._
import javatime._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

object models {

  object ModifierAction extends Enumeration {
    val Default: ModifierAction.Value = Value("DEFAULT")
    val Set: ModifierAction.Value     = Value("SET")

    implicit val decoder: Decoder[ModifierAction.Value] = Decoder.decodeEnumeration(ModifierAction)
  }

  object ModifierOptionAction extends Enumeration {
    val Default: ModifierOptionAction.Value = Value("DEFAULT")
    val Set: ModifierOptionAction.Value     = Value("SET")
    val Null: ModifierOptionAction.Value    = Value("NULL")

    implicit val decoder: Decoder[ModifierOptionAction.Value] = Decoder.decodeEnumeration(ModifierOptionAction)
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

  object EnumModifier {
    implicit def decoder[E <: Enumeration#Value: Decoder: Put]: Decoder[EnumModifier[E]] =
      deriveDecoder[EnumModifier[E]]
  }

  final case class EnumModifierOption[E <: Enumeration#Value: Put](
      action: ModifierOptionAction.Value,
      value: Option[E]
  ) extends AbstractGenericModifierOption[E](action, value)

  object EnumModifierOption {
    implicit def decoder[E <: Enumeration#Value: Decoder: Put]: Decoder[EnumModifierOption[E]] =
      deriveDecoder[EnumModifierOption[E]]
  }

  // Boolean

  final case class BooleanModifier(
      action: ModifierAction.Value,
      value: Option[Boolean]
  ) extends AbstractGenericModifier[Boolean](action, value)

  object BooleanModifier {
    implicit val decoder: Decoder[BooleanModifier] = deriveDecoder[BooleanModifier]
  }

  final case class BooleanModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[Boolean]
  ) extends AbstractGenericModifierOption[Boolean](action, value)

  object BooleanModifierOption {
    implicit val decoder: Decoder[BooleanModifierOption] = deriveDecoder[BooleanModifierOption]
  }

  // Byte

  final case class ByteModifier(
      action: ModifierAction.Value,
      value: Option[Byte]
  ) extends AbstractGenericModifier[Byte](action, value)

  object ByteModifier {
    implicit val decoder: Decoder[ByteModifier] = deriveDecoder[ByteModifier]
  }

  final case class ByteModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[Byte]
  ) extends AbstractGenericModifierOption[Byte](action, value)

  object ByteModifierOption {
    implicit val decoder: Decoder[ByteModifierOption] = deriveDecoder[ByteModifierOption]
  }

  // Array[Byte]

  final case class ByteArrayModifier(
      action: ModifierAction.Value,
      value: Option[Array[Byte]]
  ) extends AbstractGenericModifier[Array[Byte]](action, value)

  object ByteArrayModifier {
    implicit val decoder: Decoder[ByteArrayModifier] = deriveDecoder[ByteArrayModifier]
  }

  final case class ByteArrayModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[Array[Byte]]
  ) extends AbstractGenericModifierOption[Array[Byte]](action, value)

  object ByteArrayModifierOption {
    implicit val decoder: Decoder[ByteArrayModifierOption] = deriveDecoder[ByteArrayModifierOption]
  }

  // Int

  final case class IntModifier(
      action: ModifierAction.Value,
      value: Option[Int]
  ) extends AbstractGenericModifier[Int](action, value)

  object IntModifier {
    implicit val decoder: Decoder[IntModifier] = deriveDecoder[IntModifier]
  }

  final case class IntModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[Int]
  ) extends AbstractGenericModifierOption[Int](action, value)

  object IntModifierOption {
    implicit val decoder: Decoder[IntModifierOption] = deriveDecoder[IntModifierOption]
  }

  // Long

  final case class LongModifier(
      action: ModifierAction.Value,
      value: Option[Long]
  ) extends AbstractGenericModifier[Long](action, value)

  object LongModifier {
    implicit val decoder: Decoder[LongModifier] = deriveDecoder[LongModifier]
  }

  final case class LongModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[Long]
  ) extends AbstractGenericModifierOption[Long](action, value)

  object LongModifierOption {
    implicit val decoder: Decoder[LongModifierOption] = deriveDecoder[LongModifierOption]
  }

  // Double

  final case class DoubleModifier(
      action: ModifierAction.Value,
      value: Option[Double]
  ) extends AbstractGenericModifier[Double](action, value)

  object DoubleModifier {
    implicit val decoder: Decoder[DoubleModifier] = deriveDecoder[DoubleModifier]
  }

  final case class DoubleModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[Double]
  ) extends AbstractGenericModifierOption[Double](action, value)

  object DoubleModifierOption {
    implicit val decoder: Decoder[DoubleModifierOption] = deriveDecoder[DoubleModifierOption]
  }

  // BigDecimal

  final case class BigDecimalModifier(
      action: ModifierAction.Value,
      value: Option[BigDecimal]
  ) extends AbstractGenericModifier[BigDecimal](action, value)

  object BigDecimalModifier {
    implicit val decoder: Decoder[BigDecimalModifier] = deriveDecoder[BigDecimalModifier]
  }

  final case class BigDecimalModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[BigDecimal]
  ) extends AbstractGenericModifierOption[BigDecimal](action, value)

  object BigDecimalModifierOption {
    implicit val decoder: Decoder[BigDecimalModifierOption] = deriveDecoder[BigDecimalModifierOption]
  }

  // String

  final case class StringModifier(
      action: ModifierAction.Value,
      value: Option[String]
  ) extends AbstractGenericModifier[String](action, value)

  object StringModifier {
    implicit val decoder: Decoder[StringModifier] = deriveDecoder[StringModifier]
  }

  final case class StringModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[String]
  ) extends AbstractGenericModifierOption[String](action, value)

  object StringModifierOption {
    implicit val decoder: Decoder[StringModifierOption] = deriveDecoder[StringModifierOption]
  }

  // Instant

  final case class InstantModifier(
      action: ModifierAction.Value,
      value: Option[Instant]
  ) extends AbstractGenericModifier[Instant](action, value)

  object InstantModifier {
    implicit val decoder: Decoder[InstantModifier] = deriveDecoder[InstantModifier]
  }

  final case class InstantModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[Instant]
  ) extends AbstractGenericModifierOption[Instant](action, value)

  object InstantModifierOption {
    implicit val decoder: Decoder[InstantModifierOption] = deriveDecoder[InstantModifierOption]
  }

  // LocalDate

  final case class LocalDateModifier(
      action: ModifierAction.Value,
      value: Option[LocalDate]
  ) extends AbstractGenericModifier[LocalDate](action, value)

  object LocalDateModifier {
    implicit val decoder: Decoder[LocalDateModifier] = deriveDecoder[LocalDateModifier]
  }

  final case class LocalDateModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[LocalDate]
  ) extends AbstractGenericModifierOption[LocalDate](action, value)

  object LocalDateModifierOption {
    implicit val decoder: Decoder[LocalDateModifierOption] = deriveDecoder[LocalDateModifierOption]
  }

  // LocalTime

  final case class LocalTimeModifier(
      action: ModifierAction.Value,
      value: Option[LocalTime]
  ) extends AbstractGenericModifier[LocalTime](action, value)

  object LocalTimeModifier {
    implicit val decoder: Decoder[LocalTimeModifier] = deriveDecoder[LocalTimeModifier]
  }

  final case class LocalTimeModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[LocalTime]
  ) extends AbstractGenericModifierOption[LocalTime](action, value)

  object LocalTimeModifierOption {
    implicit val decoder: Decoder[LocalTimeModifierOption] = deriveDecoder[LocalTimeModifierOption]
  }

  // LocalDateTime

  final case class LocalDateTimeModifier(
      action: ModifierAction.Value,
      value: Option[LocalDateTime]
  ) extends AbstractGenericModifier[LocalDateTime](action, value)

  object LocalDateTimeModifier {
    implicit val decoder: Decoder[LocalDateTimeModifier] = deriveDecoder[LocalDateTimeModifier]
  }

  final case class LocalDateTimeModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[LocalDateTime]
  ) extends AbstractGenericModifierOption[LocalDateTime](action, value)

  object LocalDateTimeModifierOption {
    implicit val decoder: Decoder[LocalDateTimeModifierOption] = deriveDecoder[LocalDateTimeModifierOption]
  }

  // OffsetTime

  final case class OffsetTimeModifier(
      action: ModifierAction.Value,
      value: Option[OffsetTime]
  ) extends AbstractGenericModifier[OffsetTime](action, value)

  object OffsetTimeModifier {
    implicit val decoder: Decoder[OffsetTimeModifier] = deriveDecoder[OffsetTimeModifier]
  }

  final case class OffsetTimeModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[OffsetTime]
  ) extends AbstractGenericModifierOption[OffsetTime](action, value)

  object OffsetTimeModifierOption {
    implicit val decoder: Decoder[OffsetTimeModifierOption] = deriveDecoder[OffsetTimeModifierOption]
  }

  // OffsetDateTime

  final case class OffsetDateTimeModifier(
      action: ModifierAction.Value,
      value: Option[OffsetDateTime]
  ) extends AbstractGenericModifier[OffsetDateTime](action, value)

  object OffsetDateTimeModifier {
    implicit val decoder: Decoder[OffsetDateTimeModifier] = deriveDecoder[OffsetDateTimeModifier]
  }

  final case class OffsetDateTimeModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[OffsetDateTime]
  ) extends AbstractGenericModifierOption[OffsetDateTime](action, value)

  object OffsetDateTimeModifierOption {
    implicit val decoder: Decoder[OffsetDateTimeModifierOption] = deriveDecoder[OffsetDateTimeModifierOption]
  }

  // ZonedDateTime

  final case class ZonedDateTimeModifier(
      action: ModifierAction.Value,
      value: Option[ZonedDateTime]
  ) extends AbstractGenericModifier[ZonedDateTime](action, value)

  object ZonedDateTimeModifier {
    implicit val decoder: Decoder[ZonedDateTimeModifier] = deriveDecoder[ZonedDateTimeModifier]
  }

  final case class ZonedDateTimeModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[ZonedDateTime]
  ) extends AbstractGenericModifierOption[ZonedDateTime](action, value)

  object ZonedDateTimeModifierOption {
    implicit val decoder: Decoder[ZonedDateTimeModifierOption] = deriveDecoder[ZonedDateTimeModifierOption]
  }

  // Date

  final case class DateModifier(
      action: ModifierAction.Value,
      value: Option[Date]
  ) extends AbstractGenericModifier[Date](action, value)

  object DateModifier {
    implicit val decoder: Decoder[DateModifier] = deriveDecoder[DateModifier]
  }

  final case class DateModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[Date]
  ) extends AbstractGenericModifierOption[Date](action, value)

  object DateModifierOption {
    implicit val decoder: Decoder[DateModifierOption] = deriveDecoder[DateModifierOption]
  }

  // Time

  final case class TimeModifier(
      action: ModifierAction.Value,
      value: Option[Time]
  ) extends AbstractGenericModifier[Time](action, value)

  object TimeModifier {
    implicit val decoder: Decoder[TimeModifier] = deriveDecoder[TimeModifier]
  }

  final case class TimeModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[Time]
  ) extends AbstractGenericModifierOption[Time](action, value)

  object TimeModifierOption {
    implicit val decoder: Decoder[TimeModifierOption] = deriveDecoder[TimeModifierOption]
  }

  // Timestamp

  final case class TimestampModifier(
      action: ModifierAction.Value,
      value: Option[Timestamp]
  ) extends AbstractGenericModifier[Timestamp](action, value)

  object TimestampModifier {
    implicit val decoder: Decoder[TimestampModifier] = deriveDecoder[TimestampModifier]
  }

  final case class TimestampModifierOption(
      action: ModifierOptionAction.Value,
      value: Option[Timestamp]
  ) extends AbstractGenericModifierOption[Timestamp](action, value)

  object TimestampModifierOption {
    implicit val decoder: Decoder[TimestampModifierOption] = deriveDecoder[TimestampModifierOption]
  }
}
