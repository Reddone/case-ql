package com.github.reddone.caseql.circe.filter

import com.github.reddone.caseql.circe.util.decoders._
import com.github.reddone.caseql.sql.filter.primitives._
import com.github.reddone.caseql.sql.filter.wrappers.{EntityFilter, RelationFilter}
import doobie.Put
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

object decoders {

  implicit def relationFilterDecoder[A, B, FB <: EntityFilter[FB]: Decoder]: Decoder[RelationFilter[A, B, FB]] =
    deriveDecoder[RelationFilter[A, B, FB]]

  implicit def enumFilterDecoder[E <: Enumeration#Value: Decoder: Put]: Decoder[EnumFilter[E]] =
    deriveDecoder[EnumFilter[E]]

  implicit def enumFilterOptionDecoder[E <: Enumeration#Value: Decoder: Put]: Decoder[EnumFilterOption[E]] =
    deriveDecoder[EnumFilterOption[E]]

  implicit val booleanFilterDecoder: Decoder[BooleanFilter] =
    deriveDecoder[BooleanFilter]

  implicit val booleanFilterOptionDecoder: Decoder[BooleanFilterOption] =
    deriveDecoder[BooleanFilterOption]

  implicit val byteFilterDecoder: Decoder[ByteFilter] =
    deriveDecoder[ByteFilter]

  implicit val byteFilterOptionDecoder: Decoder[ByteFilterOption] =
    deriveDecoder[ByteFilterOption]

  implicit val byteArrayFilterDecoder: Decoder[ByteArrayFilter] =
    deriveDecoder[ByteArrayFilter]

  implicit val byteArrayFilterOptionDecoder: Decoder[ByteArrayFilterOption] =
    deriveDecoder[ByteArrayFilterOption]

  implicit val intFilterDecoder: Decoder[IntFilter] =
    deriveDecoder[IntFilter]

  implicit val intFilterOptionDecoder: Decoder[IntFilterOption] =
    deriveDecoder[IntFilterOption]

  implicit val longFilterDecoder: Decoder[LongFilter] =
    deriveDecoder[LongFilter]

  implicit val longFilterOptionDecoder: Decoder[LongFilterOption] =
    deriveDecoder[LongFilterOption]

  implicit val doubleFilterDecoder: Decoder[DoubleFilter] =
    deriveDecoder[DoubleFilter]

  implicit val doubleFilterOptionDecoder: Decoder[DoubleFilterOption] =
    deriveDecoder[DoubleFilterOption]

  implicit val bigDecimalFilterDecoder: Decoder[BigDecimalFilter] =
    deriveDecoder[BigDecimalFilter]

  implicit val bigDecimalFilterOptionDecoder: Decoder[BigDecimalFilterOption] =
    deriveDecoder[BigDecimalFilterOption]

  implicit val stringFilterDecoder: Decoder[StringFilter] =
    deriveDecoder[StringFilter]

  implicit val stringFilterOptionDecoder: Decoder[StringFilterOption] =
    deriveDecoder[StringFilterOption]

  implicit val instantFilterDecoder: Decoder[InstantFilter] =
    deriveDecoder[InstantFilter]

  implicit val instantFilterOptionDecoder: Decoder[InstantFilterOption] =
    deriveDecoder[InstantFilterOption]

  implicit val localDateFilterDecoder: Decoder[LocalDateFilter] =
    deriveDecoder[LocalDateFilter]

  implicit val localDateFilterOptionDecoder: Decoder[LocalDateFilterOption] =
    deriveDecoder[LocalDateFilterOption]

  implicit val localTimeFilterDecoder: Decoder[LocalTimeFilter] =
    deriveDecoder[LocalTimeFilter]

  implicit val localTimeFilterOptionDecoder: Decoder[LocalTimeFilterOption] =
    deriveDecoder[LocalTimeFilterOption]

  implicit val localDateTimeFilterDecoder: Decoder[LocalDateTimeFilter] =
    deriveDecoder[LocalDateTimeFilter]

  implicit val localDateTimeFilterOptionDecoder: Decoder[LocalDateTimeFilterOption] =
    deriveDecoder[LocalDateTimeFilterOption]

  implicit val offsetTimeFilterDecoder: Decoder[OffsetTimeFilter] =
    deriveDecoder[OffsetTimeFilter]

  implicit val offsetTimeFilterOptionDecoder: Decoder[OffsetTimeFilterOption] =
    deriveDecoder[OffsetTimeFilterOption]

  implicit val offsetDateTimeFilterDecoder: Decoder[OffsetDateTimeFilter] =
    deriveDecoder[OffsetDateTimeFilter]

  implicit val offsetDateTimeFilterOptionDecoder: Decoder[OffsetDateTimeFilterOption] =
    deriveDecoder[OffsetDateTimeFilterOption]

  implicit val zonedDateTimeFilterDecoder: Decoder[ZonedDateTimeFilter] =
    deriveDecoder[ZonedDateTimeFilter]

  implicit val zonedDateTimeFilterOptionDecoder: Decoder[ZonedDateTimeFilterOption] =
    deriveDecoder[ZonedDateTimeFilterOption]

  implicit val dateFilterDecoder: Decoder[DateFilter] =
    deriveDecoder[DateFilter]

  implicit val dateFilterOptionDecoder: Decoder[DateFilterOption] =
    deriveDecoder[DateFilterOption]

  implicit val timeFilterDecoder: Decoder[TimeFilter] =
    deriveDecoder[TimeFilter]

  implicit val timeFilterOptionDecoder: Decoder[TimeFilterOption] =
    deriveDecoder[TimeFilterOption]

  implicit val timestampFilterDecoder: Decoder[TimestampFilter] =
    deriveDecoder[TimestampFilter]

  implicit val timestampFilterOptionDecoder: Decoder[TimestampFilterOption] =
    deriveDecoder[TimestampFilterOption]
}
