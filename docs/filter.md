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

Filters should be combined together inside a case class FA in order to construct an EntityFilter[FA <: EntityFilter[FA]].

```scala
case class Test(
  field1: Int,
  field2: Option[String],
  field3: Long,
  field4: Option[Timestamp]
)

case class TestFilter(
  field1: Option[IntFilter],
  field2: Option[StringFilterOption],
  field3: Option[LongFilter],
  field4: Option[TimestampFilterOption],
  AND: Option[Seq[TestFilter]],
  OR: Option[Seq[TestFilter]],
  NOT: Option[TestFilter]
) extends EntityFilter[TestFilter]

object TestFilter {
  val empty: TestFilter = TestFilter(None, None, None, None, None, None, None)
}
```

The EntityFilter trait use a self recursive type to enable the composition of filters using AND, OR and NOT. Now, if
we want to build a where condition for a select, update or delete statement we can create the appropriate instance
of TestFilter:

```scala
val testFilter = TestFilter.empty.copy(
  field1 = Some(IntFilter.empty.copy(IN = Some(11, 12, 13))),
  field2 = Some(StringFilterOption.empty.copy(EQ = "2")),
  OR = Some(Seq(
    TestFilter.empty.copy(field1 = Some(IntFilter.empty.copy(EQ = Some(1)))),
    TestFilter.empty.copy(field3 = Some(LongFilter.empty.copy(GT = Some(3))))
  ))
)
```

The filter can also be created by deserializing the following JSON:

```json
{
  "field1": { "IN": [11, 12, 13] },
  "field2": { "EQ": "2" },
  "OR": [
    { "EQ": 1 },
    { "GT": 3 }
  ] 
}
``` 

This will produce the where condition 
"WHERE (field1 IN (11, 12, 13)) AND (field2 = '2') AND ((field1 = 1) OR (field3 > 3))".
You can create complex where conditions by nesting AND, OR and NOT as you like: it is possible to express any kind of
boolean condition using this approach.

## RelationFilter

## TableFilter

An EntityFilter[FA] with an arbitrary number of RelationFilter[A, _, _] can be used to produce a where condition in select, updare or delete queries on a Table[A, K] only
if we have an implicit instance of TableFilter[T, FA] in scope. The typeclass TableFilter guarantees that:

- an implicit TableSyntax[T] exists for T

- the filter FA has all fields of A (it can also have extra fields)

- each Filter is wrapped inside an Option

- the type of each Filter field of FA is equivalent to the type of the corresponding field in A

- each RelationFilter[A, B, FB] links A to an entity B having a filter FB with an implicit TableFilter[B, FB]

- each RelationFilter[A, B, FB] has a corresponding implicit TableLink[A, B]

These conditions are sufficient to generate a type-sage where condition. To create a table filter, use:

```scala
implicit val tableFilter: TableFilter[Test, TestFilter] = TableFilter.derive[Test, TestFilter]()
```

If the code compiles then you can be 100% sure that your filter can be used with your entity.
