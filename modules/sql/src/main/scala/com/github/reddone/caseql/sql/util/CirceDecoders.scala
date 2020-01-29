package com.github.reddone.caseql.sql.util

import java.sql.{Date, Time, Timestamp}

import io.circe.Decoder

object CirceDecoders {

  implicit val dateDecoder: Decoder[Date] = Decoder[String].map(Date.valueOf)

  implicit val timeDecoder: Decoder[Time] = Decoder[String].map(Time.valueOf)

  implicit val timestampDecoder: Decoder[Timestamp] = Decoder[String].map(Timestamp.valueOf)
}
