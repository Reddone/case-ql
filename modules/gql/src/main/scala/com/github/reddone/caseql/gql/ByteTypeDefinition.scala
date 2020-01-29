package com.github.reddone.caseql.gql

import cats.implicits._
import sangria.marshalling.BlobSupport
import sangria.schema.ScalarType
import sangria.validation.ValueCoercionViolation

import scala.util.{Failure, Success, Try}

object ByteTypeDefinition {

  // Byte

  case object ByteCoercionViolation extends ValueCoercionViolation("Byte expected (from 0 to 255)")

  def parseByte(s: String): Either[ByteCoercionViolation.type, Byte] =
    Try((s.toInt - 128).toByte) match {
      case Success(byte) ⇒ Right(byte)
      case Failure(_)    ⇒ Left(ByteCoercionViolation)
    }

  implicit val ByteType: ScalarType[Byte] =
    ScalarType[Byte](
      "Byte",
      coerceOutput = (b, caps) ⇒
        if (caps.contains(BlobSupport)) b
        else (b + 128).toString,
      coerceUserInput = {
        case s: String ⇒ parseByte(s)
        case _         ⇒ Left(ByteCoercionViolation)
      },
      coerceInput = {
        case sangria.ast.StringValue(s, _, _, _, _) ⇒ parseByte(s)
        case _                                      ⇒ Left(ByteCoercionViolation)
      }
    )

  // Array[Byte]

  private val LeftBracket  = "["
  private val RightBracket = "]"

  case object ByteArrayCoercionViolation
      extends ValueCoercionViolation("ByteArray expected ([...] of values from 0 to 255)")

  def parseByteArray(s: String): Either[ByteArrayCoercionViolation.type, Array[Byte]] =
    Try {
      if (!(s.startsWith(LeftBracket) && s.endsWith(RightBracket)))
        throw new IllegalArgumentException(s"ByteArray must start with $LeftBracket and end with $RightBracket")
      val withoutBrackets = s.substring(1, s.length - 1)
      val stringValues    = withoutBrackets.split(",").map(_.trim)
      stringValues.map(_.toInt).map(_ - 128).map(_.toByte)
    } match {
      case Success(byteArray) ⇒ Right(byteArray)
      case Failure(_)         ⇒ Left(ByteArrayCoercionViolation)
    }

  implicit val ByteArrayType: ScalarType[Array[Byte]] =
    ScalarType[Array[Byte]](
      "ByteArray",
      coerceOutput = (b, caps) ⇒
        if (caps.contains(BlobSupport)) b
        else LeftBracket + b.map(_ + 128).map(_.toString).toList.intercalate(",") + RightBracket,
      coerceUserInput = {
        case s: String ⇒ parseByteArray(s)
        case _         ⇒ Left(ByteArrayCoercionViolation)
      },
      coerceInput = {
        case sangria.ast.StringValue(s, _, _, _, _) ⇒ parseByteArray(s)
        case _                                      ⇒ Left(ByteArrayCoercionViolation)
      }
    )
}
