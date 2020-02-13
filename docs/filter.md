## Filter

In order to set the where condition inside select, update and delete statements this library uses the concept of 
*Filter*. A Filter[T] states that type T can be used to generate several conditions in a where condition. Each type
has an associated set of conditions: for example, you can use CONTAINS on a String type but not on an Int type.

Since the IS NULL condition is only possible on a nullable column, we divide filters into Filter[T] and 
Filter[Option[T]] by aliasing the latter with FilterOption[T]. If you specify more than one condition, they will be
combined using the AND operator. Since AND is the most common logical operator, I opted to use it as a default. We
will see how to use other logical operators in the next section.

For example:

```scala
val intFilter = IntFilter.empty.copy(EQ = Some(1))
val stringFilterOption = StringFilterOption.empty.copy(CONTAINS = Some("a"), IN = Some(Seq("bcbc", "abab")))
``` 
will create a filter which checks if an INT field is equal to 1 and a filter which checks if a nullable VARCHAR field
is contained in the set ("bcbc", "abab") and has an "a".

This library provides Filter[T] and FilterOption[T] for most of the types covered by Doobie *Meta[T]*. In details, the
following modifiers are provided:

- Enum and Option[Enum]: EnumFilter and EnumFilterOption

- Boolean and Option[Boolean]: BooleanFilter and BooleanFilterOption

- Byte and Option[Byte]: ByteFilter and ByteFilterOption

- Array[Byte] and Option[Array[Byte]]: ByteArrayFilter and ByteArrayFilterOption

- Int and Option[Int]: IntFilter and IntFilterOption

- Long and Option[Long]: LongFilter and LongFilterOption

- Double and Option[Double]: DoubleFilter and DoubleFilterOption

- BigDecimal and Option[BigDecimal]: BigDecimalFilter and BigDecimalFilterOption

- String and Option[String]: StringFilter and StringFilterOption

- Instant and Option[Instant]: InstantFilter and InstantFilterOption

- LocalDate and Option[LocalDate]: LocalDateFilter and LocalDateFilterOption

- LocalTime and Option[LocalTime]: LocalTimeFilter and LocalTimeFilterOption

- LocalDateTime and Option[LocalDateTime]: LocalDateTimeFilter and LocalDateTimeFilterOption

- OffsetTime and Option[OffsetTime]: OffsetTimeFilter and OffsetTimeFilterOption

- OffsetDateTime and Option[OffsetDateTime]: OffsetDateTimeFilter and OffsetDateTimeFilterOption

- ZonedDateTime and Option[ZonedDateTime]: ZonedDateTimeFilter and ZonedDateTimeFilterOption

- java.sql.Date and Option[java.sql.Date]: DateFilter and DateFilterOption

- java.sql.Time and Option[java.sql.Time]: TimeFilter and TimeFilterOption

- java.sql.Timestamp and Option[java.sql.Timestamp]: TimestampFilter and TimestampFilterOption

## EntityFilter

## RelationFilter

## TableFilter