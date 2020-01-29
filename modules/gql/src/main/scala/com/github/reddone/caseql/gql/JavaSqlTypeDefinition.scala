package com.github.reddone.caseql.gql

import java.sql.{Date, Time, Timestamp}

import sangria.marshalling.DateSupport
import sangria.schema.ScalarType
import sangria.validation.ValueCoercionViolation

import scala.util.{Failure, Success, Try}

object JavaSqlTypeDefinition {

  // Date

  case object DateCoercionViolation extends ValueCoercionViolation("Date value 'yyyy-mm-dd' expected")

  def parseDate(s: String): Either[DateCoercionViolation.type, Date] =
    Try(Date.valueOf(s)) match {
      case Success(date) ⇒ Right(date)
      case Failure(_)    ⇒ Left(DateCoercionViolation)
    }

  implicit val DateType: ScalarType[Date] =
    ScalarType[Date](
      "Date",
      coerceOutput = (d, caps) ⇒
        if (caps.contains(DateSupport)) d
        else d.toString,
      coerceUserInput = {
        case s: String ⇒ parseDate(s)
        case _         ⇒ Left(DateCoercionViolation)
      },
      coerceInput = {
        case sangria.ast.StringValue(s, _, _, _, _) ⇒ parseDate(s)
        case _                                      ⇒ Left(DateCoercionViolation)
      }
    )

  // Time

  case object TimeCoercionViolation extends ValueCoercionViolation("Time value 'hh:mm:ss' expected")

  def parseTime(s: String): Either[TimeCoercionViolation.type, Time] =
    Try(Time.valueOf(s)) match {
      case Success(time) ⇒ Right(time)
      case Failure(_)    ⇒ Left(TimeCoercionViolation)
    }

  implicit val TimeType: ScalarType[Time] =
    ScalarType[Time](
      "Time",
      coerceOutput = (d, caps) ⇒
        if (caps.contains(DateSupport)) d
        else d.toString,
      coerceUserInput = {
        case s: String ⇒ parseTime(s)
        case _         ⇒ Left(TimeCoercionViolation)
      },
      coerceInput = {
        case sangria.ast.StringValue(s, _, _, _, _) ⇒ parseTime(s)
        case _                                      ⇒ Left(TimeCoercionViolation)
      }
    )

  // Timestamp

  case object TimestampCoercionViolation
      extends ValueCoercionViolation("Timestamp value 'yyyy-mm-dd hh:mm:ss' expected")

  def parseTimestamp(s: String): Either[TimestampCoercionViolation.type, Timestamp] =
    Try(Timestamp.valueOf(s)) match {
      case Success(timestamp) ⇒ Right(timestamp)
      case Failure(_)         ⇒ Left(TimestampCoercionViolation)
    }

  implicit val TimestampType: ScalarType[Timestamp] =
    ScalarType[Timestamp](
      "Timestamp",
      coerceOutput = (d, caps) ⇒
        if (caps.contains(DateSupport)) d
        else d.toString,
      coerceUserInput = {
        case s: String ⇒ parseTimestamp(s)
        case _         ⇒ Left(TimestampCoercionViolation)
      },
      coerceInput = {
        case sangria.ast.StringValue(s, _, _, _, _) ⇒ parseTimestamp(s)
        case _                                      ⇒ Left(TimestampCoercionViolation)
      }
    )
}
