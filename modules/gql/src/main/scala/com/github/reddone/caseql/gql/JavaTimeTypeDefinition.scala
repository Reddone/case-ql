package com.github.reddone.caseql.gql

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, OffsetTime, ZonedDateTime}
import java.time.format.DateTimeFormatter

import sangria.marshalling.DateSupport
import sangria.schema.ScalarType
import sangria.validation.ValueCoercionViolation

import scala.util.{Failure, Success, Try}

object JavaTimeTypeDefinition {

  // Instant

  case object InstantCoercionViolation
      extends ValueCoercionViolation("Instant value 'yyyy-mm-dd T hh:mm:ss Z' expected")

  def parseInstant(s: String): Either[InstantCoercionViolation.type, Instant] =
    Try(Instant.parse(s)) match {
      case Success(instant) ⇒ Right(instant)
      case Failure(_)       ⇒ Left(InstantCoercionViolation)
    }

  implicit val InstantType: ScalarType[Instant] =
    ScalarType[Instant](
      "Instant",
      coerceOutput = (d, caps) ⇒
        if (caps.contains(DateSupport)) d
        else d.toString,
      coerceUserInput = {
        case s: String ⇒ parseInstant(s)
        case _         ⇒ Left(InstantCoercionViolation)
      },
      coerceInput = {
        case sangria.ast.StringValue(s, _, _, _, _) ⇒ parseInstant(s)
        case _                                      ⇒ Left(InstantCoercionViolation)
      }
    )

  // LocalDate

  private val LocalDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE

  case object LocalDateCoercionViolation extends ValueCoercionViolation("LocalDate value 'yyyy-mm-dd' expected")

  def parseLocalDate(s: String): Either[LocalDateCoercionViolation.type, LocalDate] =
    Try(LocalDate.parse(s, LocalDateFormatter)) match {
      case Success(localDate) ⇒ Right(localDate)
      case Failure(_)         ⇒ Left(LocalDateCoercionViolation)
    }

  implicit val LocalDateType: ScalarType[LocalDate] =
    ScalarType[LocalDate](
      "LocalDate",
      coerceOutput = (d, caps) ⇒
        if (caps.contains(DateSupport)) d
        else d.format(LocalDateFormatter),
      coerceUserInput = {
        case s: String ⇒ parseLocalDate(s)
        case _         ⇒ Left(LocalDateCoercionViolation)
      },
      coerceInput = {
        case sangria.ast.StringValue(s, _, _, _, _) ⇒ parseLocalDate(s)
        case _                                      ⇒ Left(LocalDateCoercionViolation)
      }
    )

  // LocalTime

  private val LocalTimeFormatter = DateTimeFormatter.ISO_LOCAL_TIME

  case object LocalTimeCoercionViolation extends ValueCoercionViolation("LocalTime value 'hh:mm:ss' expected")

  def parseLocalTime(s: String): Either[LocalTimeCoercionViolation.type, LocalTime] =
    Try(LocalTime.parse(s, LocalTimeFormatter)) match {
      case Success(localTime) ⇒ Right(localTime)
      case Failure(_)         ⇒ Left(LocalTimeCoercionViolation)
    }

  implicit val LocalTimeType: ScalarType[LocalTime] =
    ScalarType[LocalTime](
      "LocalTime",
      coerceOutput = (d, caps) ⇒
        if (caps.contains(DateSupport)) d
        else d.format(LocalTimeFormatter),
      coerceUserInput = {
        case s: String ⇒ parseLocalTime(s)
        case _         ⇒ Left(LocalTimeCoercionViolation)
      },
      coerceInput = {
        case sangria.ast.StringValue(s, _, _, _, _) ⇒ parseLocalTime(s)
        case _                                      ⇒ Left(LocalTimeCoercionViolation)
      }
    )

  // LocalDateTime

  private val LocalDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME

  case object LocalDateTimeCoercionViolation
      extends ValueCoercionViolation("LocalDateTime value 'yyyy-mm-dd T hh:mm:ss' expected")

  def parseLocalDateTime(s: String): Either[LocalDateTimeCoercionViolation.type, LocalDateTime] =
    Try(LocalDateTime.parse(s, LocalDateTimeFormatter)) match {
      case Success(localDateTime) ⇒ Right(localDateTime)
      case Failure(_)             ⇒ Left(LocalDateTimeCoercionViolation)
    }

  implicit val LocalDateTimeType: ScalarType[LocalDateTime] =
    ScalarType[LocalDateTime](
      "LocalDateTime",
      coerceOutput = (d, caps) ⇒
        if (caps.contains(DateSupport)) d
        else d.format(LocalDateTimeFormatter),
      coerceUserInput = {
        case s: String ⇒ parseLocalDateTime(s)
        case _         ⇒ Left(LocalDateTimeCoercionViolation)
      },
      coerceInput = {
        case sangria.ast.StringValue(s, _, _, _, _) ⇒ parseLocalDateTime(s)
        case _                                      ⇒ Left(LocalDateTimeCoercionViolation)
      }
    )

  // OffsetTime

  private val OffsetTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME

  case object OffsetTimeCoercionViolation extends ValueCoercionViolation("OffsetTime value 'hh:mm:ss+hh:mm' expected")

  def parseOffsetTime(s: String): Either[OffsetTimeCoercionViolation.type, OffsetTime] =
    Try(OffsetTime.parse(s, OffsetTimeFormatter)) match {
      case Success(offsetTime) ⇒ Right(offsetTime)
      case Failure(_)          ⇒ Left(OffsetTimeCoercionViolation)
    }

  implicit val OffsetTimeType: ScalarType[OffsetTime] =
    ScalarType[OffsetTime](
      "OffsetTime",
      coerceOutput = (d, caps) ⇒
        if (caps.contains(DateSupport)) d
        else d.format(OffsetTimeFormatter),
      coerceUserInput = {
        case s: String ⇒ parseOffsetTime(s)
        case _         ⇒ Left(OffsetTimeCoercionViolation)
      },
      coerceInput = {
        case sangria.ast.StringValue(s, _, _, _, _) ⇒ parseOffsetTime(s)
        case _                                      ⇒ Left(OffsetTimeCoercionViolation)
      }
    )

  // OffsetDateTime

  private val OffsetDateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME

  case object OffsetDateTimeCoercionViolation
      extends ValueCoercionViolation("OffsetDateTime value 'yyyy-mm-dd T hh:mm:ss+hh:mm' expected")

  def parseOffsetDateTime(s: String): Either[OffsetDateTimeCoercionViolation.type, OffsetDateTime] =
    Try(OffsetDateTime.parse(s, OffsetDateTimeFormatter)) match {
      case Success(offsetDateTime) ⇒ Right(offsetDateTime)
      case Failure(_)              ⇒ Left(OffsetDateTimeCoercionViolation)
    }

  implicit val OffsetDateTimeType: ScalarType[OffsetDateTime] =
    ScalarType[OffsetDateTime](
      "OffsetDateTime",
      coerceOutput = (d, caps) ⇒
        if (caps.contains(DateSupport)) d
        else d.format(OffsetDateTimeFormatter),
      coerceUserInput = {
        case s: String ⇒ parseOffsetDateTime(s)
        case _         ⇒ Left(OffsetDateTimeCoercionViolation)
      },
      coerceInput = {
        case sangria.ast.StringValue(s, _, _, _, _) ⇒ parseOffsetDateTime(s)
        case _                                      ⇒ Left(OffsetDateTimeCoercionViolation)
      }
    )

  // ZonedDateTime

  private val ZonedDateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME

  case object ZonedDateTimeCoercionViolation
      extends ValueCoercionViolation("ZonedDateTime value 'yyyy-mm-dd T hh:mm:ss+hh:mm[ZoneID]' expected")

  def parseZonedDateTime(s: String): Either[ZonedDateTimeCoercionViolation.type, ZonedDateTime] =
    Try(ZonedDateTime.parse(s, ZonedDateTimeFormatter)) match {
      case Success(zonedDateTime) ⇒ Right(zonedDateTime)
      case Failure(_)             ⇒ Left(ZonedDateTimeCoercionViolation)
    }

  implicit val ZonedDateTimeType: ScalarType[ZonedDateTime] =
    ScalarType[ZonedDateTime](
      "ZonedDateTime",
      coerceOutput = (d, caps) ⇒
        if (caps.contains(DateSupport)) d
        else d.format(ZonedDateTimeFormatter),
      coerceUserInput = {
        case s: String ⇒ parseZonedDateTime(s)
        case _         ⇒ Left(ZonedDateTimeCoercionViolation)
      },
      coerceInput = {
        case sangria.ast.StringValue(s, _, _, _, _) ⇒ parseZonedDateTime(s)
        case _                                      ⇒ Left(ZonedDateTimeCoercionViolation)
      }
    )
}
