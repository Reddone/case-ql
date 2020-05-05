package com.github.reddone.caseql.circe.modifier

import com.github.reddone.caseql.circe.util.decoders._
import com.github.reddone.caseql.sql.modifier.primitives._
import doobie.Put
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

object decoders {

  implicit val modifierActionDecoder: Decoder[ModifierAction.Value] =
    Decoder.decodeEnumeration(ModifierAction)

  implicit val modifierOptionActionDecoder: Decoder[ModifierOptionAction.Value] =
    Decoder.decodeEnumeration(ModifierOptionAction)

  implicit def enumModifierDecoder[E <: Enumeration#Value: Decoder: Put]: Decoder[EnumModifier[E]] =
    deriveDecoder[EnumModifier[E]]

  implicit def enumModifierOptionDecoder[E <: Enumeration#Value: Decoder: Put]: Decoder[EnumModifierOption[E]] =
    deriveDecoder[EnumModifierOption[E]]

  implicit val booleanModifierDecoder: Decoder[BooleanModifier] =
    deriveDecoder[BooleanModifier]

  implicit val booleanModifierOptionDecoder: Decoder[BooleanModifierOption] =
    deriveDecoder[BooleanModifierOption]

  implicit val byteModifierDecoder: Decoder[ByteModifier] =
    deriveDecoder[ByteModifier]

  implicit val byteModifierOptionDecoder: Decoder[ByteModifierOption] =
    deriveDecoder[ByteModifierOption]

  implicit val byteArrayModifierDecoder: Decoder[ByteArrayModifier] =
    deriveDecoder[ByteArrayModifier]

  implicit val byteArrayModifierOptionDecoder: Decoder[ByteArrayModifierOption] =
    deriveDecoder[ByteArrayModifierOption]

  implicit val intModifierDecoder: Decoder[IntModifier] =
    deriveDecoder[IntModifier]

  implicit val intModifierOptionDecoder: Decoder[IntModifierOption] =
    deriveDecoder[IntModifierOption]

  implicit val longModifierDecoder: Decoder[LongModifier] =
    deriveDecoder[LongModifier]

  implicit val longModifierOptionDecoder: Decoder[LongModifierOption] =
    deriveDecoder[LongModifierOption]

  implicit val doubleModifierDecoder: Decoder[DoubleModifier] =
    deriveDecoder[DoubleModifier]

  implicit val doubleModifierOptionDecoder: Decoder[DoubleModifierOption] =
    deriveDecoder[DoubleModifierOption]

  implicit val bigDecimalModifierDecoder: Decoder[BigDecimalModifier] =
    deriveDecoder[BigDecimalModifier]

  implicit val bigDecimalModifierOptionDecoder: Decoder[BigDecimalModifierOption] =
    deriveDecoder[BigDecimalModifierOption]

  implicit val stringModifierDecoder: Decoder[StringModifier] =
    deriveDecoder[StringModifier]

  implicit val stringModifierOptionDecoder: Decoder[StringModifierOption] =
    deriveDecoder[StringModifierOption]

  implicit val instantModifierDecoder: Decoder[InstantModifier] =
    deriveDecoder[InstantModifier]

  implicit val instantModifierOptionDecoder: Decoder[InstantModifierOption] =
    deriveDecoder[InstantModifierOption]

  implicit val localDateModifierDecoder: Decoder[LocalDateModifier] =
    deriveDecoder[LocalDateModifier]

  implicit val localDateModifierOptionDecoder: Decoder[LocalDateModifierOption] =
    deriveDecoder[LocalDateModifierOption]

  implicit val localTimeModifierDecoder: Decoder[LocalTimeModifier] =
    deriveDecoder[LocalTimeModifier]

  implicit val localTimeModifierOptionDecoder: Decoder[LocalTimeModifierOption] =
    deriveDecoder[LocalTimeModifierOption]

  implicit val localDateTimeModifierDecoder: Decoder[LocalDateTimeModifier] =
    deriveDecoder[LocalDateTimeModifier]

  implicit val localDatetimeModifierOptionDecoder: Decoder[LocalDateTimeModifierOption] =
    deriveDecoder[LocalDateTimeModifierOption]

  implicit val offsetTimeModifierDecoder: Decoder[OffsetTimeModifier] =
    deriveDecoder[OffsetTimeModifier]

  implicit val offsetTimeModifierOptionDecoder: Decoder[OffsetTimeModifierOption] =
    deriveDecoder[OffsetTimeModifierOption]

  implicit val offsetDateTimeModifierDecoder: Decoder[OffsetDateTimeModifier] =
    deriveDecoder[OffsetDateTimeModifier]

  implicit val offsetDateTimeModifierOptionDecoder: Decoder[OffsetDateTimeModifierOption] =
    deriveDecoder[OffsetDateTimeModifierOption]

  implicit val zonedDateTimeModifierDecoder: Decoder[ZonedDateTimeModifier] =
    deriveDecoder[ZonedDateTimeModifier]

  implicit val zonedDateTimeModifierOptionDecoder: Decoder[ZonedDateTimeModifierOption] =
    deriveDecoder[ZonedDateTimeModifierOption]

  implicit val dateModifierDecoder: Decoder[DateModifier] =
    deriveDecoder[DateModifier]

  implicit val dateModifierOptionDecoder: Decoder[DateModifierOption] =
    deriveDecoder[DateModifierOption]

  implicit val timeModifierDecoder: Decoder[TimeModifier] =
    deriveDecoder[TimeModifier]

  implicit val timeModifierOptionDecoder: Decoder[TimeModifierOption] =
    deriveDecoder[TimeModifierOption]

  implicit val timestampModifierDecoder: Decoder[TimestampModifier] =
    deriveDecoder[TimestampModifier]

  implicit val timestampModifierOptionDecoder: Decoder[TimestampModifierOption] =
    deriveDecoder[TimestampModifierOption]
}
