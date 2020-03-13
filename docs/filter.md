## Filter

In order to set the where condition inside select, update and delete statements this library uses the concept of 
*Filter*. A *Filter[T]* states that type *T* can be used to generate several conditions in a where condition. Each type
has an associated set of conditions: for example, you can use CONTAINS on a *String* type but not on an *Int* type.

Since the IS NULL condition is only possible on a nullable column, we divide filters into *Filter[T]* and 
*Filter[Option[T]]* by aliasing the latter with *FilterOption[T]*. If you specify more than one condition, they will be
combined using the AND operator. Since AND is the most common logical operator, I opted to use it as a default. We
will see how to use other logical operators in the next section.

For example:

```scala
val intFilter = IntFilter.empty.copy(EQ = Some(1))
val stringFilterOption = StringFilterOption.empty.copy(CONTAINS = Some("a"), IN = Some(Seq("bcbc", "abab")))
```
 
will create a filter which checks if an INT field is equal to 1 and a filter which checks if a nullable VARCHAR field
is contained in the set ("bcbc", "abab") and has an "a".

This library provides *Filter[T]* and *FilterOption[T]* for most of the types covered by Doobie *Meta[T]*. 
In details, the following filters are provided:

- *Enum* and *Option[Enum]*: *EnumFilter* and *EnumFilterOption*

- *Boolean* and *Option[Boolean]*: *BooleanFilter* and *BooleanFilterOption*

- *Byte* and *Option[Byte]*: *ByteFilter* and *ByteFilterOption*

- *Array[Byte]* and *Option[Array[Byte]]*: *ByteArrayFilter* and *ByteArrayFilterOption*

- *Int* and *Option[Int]*: *IntFilter* and *IntFilterOption*

- *Long* and *Option[Long]*: *LongFilter* and *LongFilterOption*

- *Double* and *Option[Double]*: *DoubleFilter* and *DoubleFilterOption*

- *BigDecimal* and *Option[BigDecimal]*: *BigDecimalFilter* and *BigDecimalFilterOption*

- *String* and *Option[String]*: *StringFilter* and *StringFilterOption*

- *Instant* and *Option[Instant]*: *InstantFilter* and *InstantFilterOption*

- *LocalDate* and *Option[LocalDate]*: *LocalDateFilter* and *LocalDateFilterOption*

- *LocalTime* and *Option[LocalTime]*: *LocalTimeFilter* and *LocalTimeFilterOption*

- *LocalDateTime* and *Option[LocalDateTime]*: *LocalDateTimeFilter* and *LocalDateTimeFilterOption*

- *OffsetTime* and *Option[OffsetTime]*: *OffsetTimeFilter* and *OffsetTimeFilterOption*

- *OffsetDateTime* and *Option[OffsetDateTime]*: *OffsetDateTimeFilter* and *OffsetDateTimeFilterOption*

- *ZonedDateTime* and *Option[ZonedDateTime]*: *ZonedDateTimeFilter* and *ZonedDateTimeFilterOption*

- *java.sql.Date* and *Option[java.sql.Date]*: *DateFilter* and *DateFilterOption*

- *java.sql.Time* and *Option[java.sql.Time]*: *TimeFilter* and *TimeFilterOption*

- *java.sql.Timestamp* and *Option[java.sql.Timestamp]*: *TimestampFilter* and *TimestampFilterOption*

## EntityFilter

Filters should be combined together inside a case class *FA* extending *EntityFilter[FA <: EntityFilter[FA]]*:

```scala
case class Test(
  field1: Int,
  field2: Option[String],
  field3: Long,
  field4: Option[Timestamp]
)
case class TestKey(
  field1: Int,
  field3: Long
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

implicit val testTable: Table[Test, TestKey] = Table.derive[Test, TestKey]()
```

The *EntityFilter* trait use a self recursive type to enable the composition of filters using AND, OR and NOT. Now, if
we want to build a where condition for a select, update or delete statement we can create the appropriate instance
of TestFilter:

```scala
val testFilter = TestFilter.empty.copy(
  field1 = Some(IntFilter.empty.copy(IN = Some(11, 12, 13))),
  field2 = Some(StringFilterOption.empty.copy(EQ = Some("2"))),
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
"WHERE (test.field1 IN (11, 12, 13)) AND (test.field2 = '2') AND ((test.field1 = 1) OR (test.field3 > 3))".
You can create complex where conditions by nesting AND, OR and NOT as you like: it is possible to express any kind of
boolean condition using this approach. The NOT operator acts on a single filter, while the AND and OR operators act
on a sequence of filters. 

## RelationFilter

If you want to filter entities according to criteria on their relation, then you have to use a 
RelationFilter[A, B, FB <: EntityFilter[FB]] in order to allow an EntityFilter FB to be used on entity A linked to 
entity B. Basically, RelationFilter acts as a wrapper for an EntityFilter, allowing the latter to be used inside
other filters.
Consider the following example:

```scala
case class TestDirect(
  field1: String,
  field2: Option[Long],
  field3: Timestamp,
  testField1: Int
)
case class TestDirectKey(
  field1: String
)

case class TestDirectFilter(
  field1: Option[StringFilter],
  field2: Option[LongFilterOption],
  field3: Option[TimestampFilter],
  relationTest: Option[RelationFilter[TestDirect, Test, TestFilter]],
  AND: Option[Seq[TestDirectFilter]],
  OR: Option[Seq[TestDirectFilter]],
  NOT: Option[TestDirectFilter]
) extends EntityFilter[TestDirectFilter]

object TestDirectFilter {
  val empty = TestDirectFilter(None, None, None, None, None, None, None)
}

implicit val testDirectTable: Table[TestDirect, TestDirectKey] = Table.derive[TestDirect, TestDirectKey]()

implicit val testTestDirectLink: TableLink[Test, TestDirect] = TableLink.direct(testTable, testDirectTable)(
  (a, b) => NonEmptyList.of(("field1", "testField1"))
)
```

Here TestFilter is wrapped by RelationFilter, which also adds a type information regarding the link between entities.
This type information is necessary in order to help the TableFilter derivation process, so be careful to provide types
in the right order. Note that the field holding the RelationFilter is named "relationTest", but you can give it the
name you prefer. It makes little sense to enforce a naming convention on relations because one cannot know in advance
which relations will be included in an EntityFilter; for example, you can opt to don't use RelationFilter at all.

The RelationFilter case class has three fields: EVERY, SOME and NONE. The first one checks that every related entity
meets the filter conditions, the second one performs the check on at least one related entity and the third one checks
that no related entity meets the filter conditions.
The following:

```scala
val testDirectFilter = TestDirectFilter.empty.copy(
  field2 = Some(LongFilterOption.copy(EQ = Some(1L))),
  relationTest = Some(
    RelationFilter.empty[TestDirect, Test, TestFilter].copy(
      SOME = Some(
        TestFilter.empty.copy(field1 = Some(IntFilter.empty.copy(IN = Some(Seq(11, 12, 13)))))
      )
    )
  )
)
```

or its serialized form:

```json
{
  "field2": { "EQ":  1 },
  "relationTest": {
    "SOME": {
      "field1": { "IN": [11, 12, 13] }
    }
  }
}
```

will produce the where condition 
"WHERE (test_direct.field2 = 1) AND 
(EXISTS (SELECT 1 FROM test_direct WHERE test.field1 = test_direct.testField1 AND test_direct.field1 IN (11, 12, 13)))".
The operators EVERY, SOME and NONE accept a single filter.

## TableFilter

An EntityFilter[FA] with an arbitrary number of RelationFilter[A, _, _] can be used to produce a where condition in select, 
update or delete queries on a Table[A, K] only
if we have an implicit instance of TableFilter[T, FA] in scope. The typeclass TableFilter guarantees that:

- an implicit TableSyntax[T] exists for T

- the filter FA has all fields of A (it can also have extra fields)

- each Filter is wrapped inside an Option

- the type of each Filter field of FA is equivalent to the type of the corresponding field in A

- each RelationFilter[A, B, FB] links A to an entity B having a filter FB with an implicit TableFilter[B, FB]

- each RelationFilter[A, B, FB] has a corresponding implicit TableLink[A, B]

These conditions are sufficient to generate a type-safe where condition. To create a table filter, use:

```scala
implicit val tableFilter: TableFilter[Test, TestFilter] = TableFilter.derive[Test, TestFilter]()
```

If the code compiles then you can be 100% sure that your filter can be used with your entity.
