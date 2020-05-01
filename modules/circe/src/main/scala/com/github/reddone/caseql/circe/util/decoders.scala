package com.github.reddone.caseql.circe.util

import java.sql.{Date, Time, Timestamp}
import java.time._
import java.time.temporal.Temporal

import io.circe.Decoder
import shapeless.{cachedImplicit, TypeOf}

object decoders {

  implicit val booleanDecoder: TypeOf.`Decoder[Boolean]`.type = cachedImplicit

  implicit val byteDecoder: TypeOf.`Decoder[Byte]`.type = cachedImplicit

  implicit val byteArrayDecoder: TypeOf.`Decoder[Array[Byte]]`.type = cachedImplicit

  implicit val intDecoder: TypeOf.`Decoder[Int]`.type = cachedImplicit

  implicit val longDecoder: TypeOf.`Decoder[Long]`.type = cachedImplicit

  implicit val doubleDecoder: TypeOf.`Decoder[Double]`.type = cachedImplicit

  implicit val bigDecimalDecoder: TypeOf.`Decoder[BigDecimal]`.type = cachedImplicit

  implicit val stringDecoder: TypeOf.`Decoder[String]`.type = cachedImplicit

  implicit val instantDecoder: TypeOf.`Decoder[Instant]`.type = cachedImplicit

  implicit val localDateDecoder: TypeOf.`Decoder[LocalDate]`.type = cachedImplicit

  implicit val localTimeDecoder: TypeOf.`Decoder[LocalTime]`.type = cachedImplicit

  implicit val localDateTimeDecoder: TypeOf.`Decoder[LocalDateTime]`.type = cachedImplicit

  implicit val offsetTimeDecoder: TypeOf.`Decoder[OffsetTime]`.type = cachedImplicit

  implicit val offsetDateTimeDecoder: TypeOf.`Decoder[OffsetDateTime]`.type = cachedImplicit

  implicit val zonedDateTimeDecoder: TypeOf.`Decoder[ZonedDateTime]`.type = cachedImplicit

  implicit val dateDecoder: Decoder[Date] = Decoder[String].map(Date.valueOf)

  implicit val timeDecoder: Decoder[Time] = Decoder[String].map(Time.valueOf)

  implicit val timestampDecoder: Decoder[Timestamp] = Decoder[String].map(Timestamp.valueOf)
}
