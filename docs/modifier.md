## Modifier

In order to set values inside insert and update statement this library uses the concept of a *Modifier*. When setting
a value in SQL, we have three different choices: set it to NULL if the column is nullable, set it to its default
value or set it to a value provided by the user. A *Modifier[A]* states that type *A* can be used to perform such
operation on a particular column.

Since it is not possible to set a NULL value on a non nullable column, we divide modifiers into Modifier[A] and
Modifier[Option[A]] by aliasing the latter as ModifierOption[A]. A modifier always contains two fields, i.e. action
and value: the former specify a choice on the column and the latter specify the value if you have opted to set a value.
For example:

```scala
val intModifier          = IntModifier(ModifierAction.Set, Some(1))              // insert or set 1
val stringModifierOption = StringModifierOption(ModifierOptionAction.Null, None) // insert or set NULL
```

This library provides Modifier[A] and ModifierOption[A] for most of the types covered by Doobie Meta. In details, the
following modifiers are provided:

- Enum and Option[Enum]: EnumModifier and EnumModifierOption

- Boolean and Option[Boolean]: BooleanModifier and BooleanModifierOption

- Byte and Option[Byte]: ByteModifier and ByteModifierOption

- Array[Byte] and Option[Array[Byte]]: ByteArrayModifier and ByteArrayModifierOption

- Int and Option[Int]: IntModifier and IntModifierOption

- Long and Option[Long]: LongModifier and LongModifierOption

- Double and Option[Double]: DoubleModifier and DoubleModifierOption

- BigDecimal and Option[BigDecimal]: BigDecimalModifier and BigDecimalModifierOption

- String and Option[String]: StringModifier and StringModifierOption

- Instant and Option[Instant]: InstantModifier and InstantModifierOption

- LocalDate and Option[LocalDate]: LocalDateModifier and LocalDateModifierOption

- LocalTime and Option[LocalTime]: LocalTimeModifier and LocalTimeModifierOption

- LocalDateTime and Option[LocalDateTime]: LocalDateTimeModifier and LocalDateTimeModifierOption

- OffsetTime and Option[OffsetTime]: OffsetTimeModifier and OffsetTimeModifierOption

- OffsetDateTime and Option[OffsetDateTime]: OffsetDateTimeModifier and OffsetDateTimeModifierOption

- ZonedDateTime and Option[ZonedDateTime]: ZonedDateTimeModifier and ZonedDateTimeModifierOption

- java.sql.Date and Option[java.sql.Date]: DateModifier and DateModifierOption

- java.sql.Time and Option[java.sql.Time]: TimeModifier and TimeModifierOption

- java.sql.Timestamp and Option[java.sql.Timestamp]: TimestampModifier and TimestampModifierOption

## EntityModifier

Modifiers should be combined together inside a case class in order to construct an EntityModifier:

```scala
case class Test(
  field1: Int,
  field2: Option[String],
  field3: Long,
  field4: Option[Timestamp]
)

case class TestModifier(
  field1: Option[IntModifier],
  field2: Option[StringModifierOption],
  field3: Option[LongModifier],
  field4: Option[TimestampModifierOption]
) extends EntityModifier[TestModifier]

object TestModifier {
  val empty: TestModifier = TestModifier(None, None, None, None)
}
```

At the moment the EntityModifier trait is used only as a marker, whereas EntityFilter plays an active
role in the context of filters. Now, if we want to build a dynamic insert statement we can create an appropriate
instance of TestModifier:

```scala
val testModifier = TestModifier.empty.copy( 
  field2 = StringModifierOption(ModifierOptionAction.Set, Some("2")),
  field3 = LongModifier(ModifierAction.Set, Some(3)),
  field4 = TimestampModifierOption(ModifierOptionAction.Null, None)
)
``` 

This will produce a statement like "INSERT INTO test (field1, field2, field3, field4) VALUES"

## TableModifier
