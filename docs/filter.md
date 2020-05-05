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
of *TestFilter*:

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
    { "field1": { "EQ": 1 } },
    { "field3": { "GT": 3 } }
  ] 
}
``` 

This will produce the where condition:

```sql
WHERE (test.field1 IN (11, 12, 13)) 
    AND (test.field2 = '2') 
    AND ((test.field1 = 1) OR (test.field3 > 3))".
```

You can create complex where conditions by nesting AND, OR and NOT as you like: it is possible to express any kind of
boolean condition using this approach. The NOT operator acts on a single filter, while the AND and OR operators act
on a sequence of filters. 

## RelationFilter

If you want to filter entities according to criteria on their relation, then you have to use a 
*RelationFilter[A, B, FB <: EntityFilter[FB]]* in order to allow an *EntityFilter[FB]* to be used on entity *A* 
linked to entity *B*. Basically, *RelationFilter* acts as a wrapper for an *EntityFilter*, allowing the latter to be 
used inside other filters.
Consider the following example which uses tables and links from the table documentation:

```scala
// filter for left table with self relation and junction relation with right
case class TestLeftFilter(
    field1: Option[IntFilter],
    field2: Option[StringFilter],
    field3: Option[IntFilter],
    selfRelation: Option[RelationFilter[TestLeft, TestLeft, TestLeftFilter]],
    rightRelation: Option[RelationFilter[TestLeft, TestRight, TestRightFilter]],
    AND: Option[Seq[TestLeftFilter]],
    OR: Option[Seq[TestLeftFilter]],
    NOT: Option[TestLeftFilter]
) extends EntityFilter[TestLeftFilter]
object TestLeftFilter {
  val empty: TestLeftFilter = TestLeftFilter(None, None, None, None, None, None, None, None)
}
// filter for direct table with direct relation with left
case class TestDirectFilter(
    field1: Option[StringFilter],
    field2: Option[TimestampFilter],
    field3: Option[IntFilter],
    leftRelation: Option[RelationFilter[TestDirect, TestLeft, TestLeftFilter]],
    AND: Option[Seq[TestDirectFilter]],
    OR: Option[Seq[TestDirectFilter]],
    NOT: Option[TestDirectFilter]
) extends EntityFilter[TestDirectFilter]
object TestDirectFilter {
  val empty: TestDirectFilter = TestDirectFilter(None, None, None, None, None, None, None)
}
// filter for right table with junction relation with left
case class TestRightFilter(
    field1: Option[LongFilter],
    field2: Option[StringFilter],
    field3: Option[IntFilter],
    leftRelation: Option[RelationFilter[TestRight, TestLeft, TestLeftFilter]],
    AND: Option[Seq[TestRightFilter]],
    OR: Option[Seq[TestRightFilter]],
    NOT: Option[TestRightFilter]
) extends EntityFilter[TestRightFilter]
object TestRightFilter {
  val empty: TestRightFilter = TestRightFilter(None, None, None, None, None, None, None)
}
```

Let's take *TestLeftFilter* as an example: here, the two *RelationFilter* fields are used to add a type information
regarding the link between entities, in order to help the *TableFilter* derivation process. Be careful to provide types
in the right order or the derivation will not work. The fields holding relations are named "selfRelation" and 
"rightRelation" but you can use the name you like, since there is no check on it mainly because we cannot know in 
advance how many relations you want to use in the filter; for example, you can opt to not use *RelationFilter* at all.

The *RelationFilter* case class has three fields: EVERY, SOME and NONE. The first one checks that every related entity
meets the filter conditions, the second one performs the check on at least one related entity and the third one checks
that no related entity meets the filter conditions.

The following:

```scala
val testDirectFilter = TestDirectFilter.empty.copy(
  field1 = Some(StringFilter.copy(EQ = Some("1"))),
  leftRelation = Some(
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
  "field1": { "EQ":  "1" },
  "leftRelation": {
    "SOME": {
      "field1": { "IN": [11, 12, 13] }
    }
  }
}
```

will produce the where condition :

```sql
WHERE (test_direct.field1 = '1') 
    AND (EXISTS (
         SELECT 1 
         FROM (SELECT * FROM test_left WHERE test_left.field1 IN (11, 12, 13)) AS test_left
         WHERE test_left.field1 = test_direct.field3 
    ))".
```

The operators EVERY, SOME and NONE accept a single filter.

## TableFilter

An *EntityFilter[FA]* with an arbitrary number of *RelationFilter[A, B, FB]* can be used to produce a where condition 
in a select, update or delete queries on a *Table[A, K]* only if we have an implicit instance of *TableFilter[A, FA]* 
in scope. The typeclass *TableFilter* guarantees that:

- an implicit *TableSyntax[A]* exists for *A*

- the filter *FA* has all or a subset of the fields of *A*

- each *Filter* is wrapped inside an *Option*

- the type of each *Filter* field of *FA* is equivalent to the type of the corresponding field in *A*

- each *RelationFilter[A, B, FB]* links *A* to an entity *B* having a filter *FB* with an implicit *TableFilter[B, FB]*

- each *RelationFilter[A, B, FB]* has a corresponding implicit *TableLink[A, B]*

These conditions are sufficient to generate a type-safe where condition. To create a table filter, use:

```scala
implicit val tableFilter: TableFilter[Test, TestFilter] = TableFilter.derive[Test, TestFilter]()
```

or in the case of relations:

```scala
implicit lazy val leftTableFilter: TableFilter[TestLeft, TestLeftFilter] =
    TableFilter.derive[TestLeft, TestLeftFilter]()

implicit lazy val rightTableFilter: TableFilter[TestRight, TestRightFilter] =
    TableFilter.derive[TestRight, TestRightFilter]()

implicit val directTableFilter: TableFilter[TestDirect, TestDirectFilter] =
    TableFilter.derive[TestDirect, TestDirectFilter]()
```

The use of "implicit lazy val" is necessary because there is a circular reference between left and right filters, i.e.
the right filter can use the left filter and the right filter can use the left filter. This is not uncommon when
dealing with circular references in implicit derivation.

If the code compiles then you can be 100% sure that your filter can be used with your entity.
