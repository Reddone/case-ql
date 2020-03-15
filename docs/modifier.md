## Modifier

In order to set values inside insert and update statement this library uses the concept of a *Modifier*. When setting
a value in SQL, we have three different choices: set it to NULL if the column is nullable, set it to DEFAULT
or set it to a value provided by the user. A *Modifier[T]* states that type *T* can be used to perform such
operation on a particular column.

Since it is not possible to set a NULL value on a non nullable column, we divide modifiers into *Modifier[T]* and
*Modifier[Option[T]]* by aliasing the latter as *ModifierOption[T]*. A modifier always contains two fields, i.e. action
and value: the former specify a choice on the column and the latter specify the value if you have opted to set a value.

For example:

```scala
val intModifier          = IntModifier(ModifierAction.Set, Some(1))              // insert or set 1
val stringModifierOption = StringModifierOption(ModifierOptionAction.Null, None) // insert or set NULL
```

will create a modifier which set an INT field to 1 and a modifier which set a nullable VARCHAR field to NULL.

This library provides *Modifier[A]* and *ModifierOption[A]* for most of the types covered by Doobie *Meta[T]*. 
In details, the following modifiers are provided:

- *Enum* and *Option[Enum]*: *EnumModifier* and *EnumModifierOption*

- *Boolean* and *Option[Boolean]*: *BooleanModifier* and *BooleanModifierOption*

- *Byte* and *Option[Byte]*: *ByteModifier* and *ByteModifierOption*

- *Array[Byte]* and *Option[Array[Byte]]*: *ByteArrayModifier* and *ByteArrayModifierOption*

- *Int* and *Option[Int]*: *IntModifier* and *IntModifierOption*

- *Long* and *Option[Long]*: *LongModifier* and *LongModifierOption*

- *Double* and *Option[Double]*: *DoubleModifier* and *DoubleModifierOption*

- *BigDecimal* and *Option[BigDecimal]*: *BigDecimalModifier* and *BigDecimalModifierOption*

- *String* and *Option[String]*: *StringModifier* and *StringModifierOption*

- *Instant* and *Option[Instant]*: *InstantModifier* and *InstantModifierOption*

- *LocalDate* and *Option[LocalDate]*: *LocalDateModifier* and *LocalDateModifierOption*

- *LocalTime* and *Option[LocalTime]*: *LocalTimeModifier* and *LocalTimeModifierOption*

- *LocalDateTime* and *Option[LocalDateTime]*: *LocalDateTimeModifier* and *LocalDateTimeModifierOption*

- *OffsetTime* and *Option[OffsetTime]*: *OffsetTimeModifier* and *OffsetTimeModifierOption*

- *OffsetDateTime* and *Option[OffsetDateTime]*: *OffsetDateTimeModifier* and *OffsetDateTimeModifierOption*

- *ZonedDateTime* and *Option[ZonedDateTime]*: *ZonedDateTimeModifier* and *ZonedDateTimeModifierOption*

- *java.sql.Date* and *Option[java.sql.Date]*: *DateModifier* and *DateModifierOption*

- *java.sql.Time* and *Option[java.sql.Time]*: *TimeModifier* and *TimeModifierOption*

- *java.sql.Timestamp* and *Option[java.sql.Timestamp]*: *TimestampModifier* and *TimestampModifierOption*

## EntityModifier

Modifiers should be combined together inside a case class *MA* extending *EntityModifier[MA <: EntityModifier[MA]]*:

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

case class TestModifier(
  field1: Option[IntModifier],
  field2: Option[StringModifierOption],
  field3: Option[LongModifier],
  field4: Option[TimestampModifierOption]
) extends EntityModifier[TestModifier]

object TestModifier {
  val empty: TestModifier = TestModifier(None, None, None, None)
}

implicit val testTable: Table[Test, TestKey] = Table.derive[Test, TestKey]()
```

At the moment the *EntityModifier* trait is used only as a marker, whereas *EntityFilter* plays an active
role in the context of filters. Now, if we want to build a dynamic insert or update statement we can create an appropriate
instance of *TestModifier*:

```scala
val testModifier = TestModifier.empty.copy( 
  field2 = Some(StringModifierOption(ModifierOptionAction.Set, Some("2"))),
  field3 = Some(LongModifier(ModifierAction.Set, Some(3))),
  field4 = Some(TimestampModifierOption(ModifierOptionAction.Null, None))
)
```

The modifier can also be created by deserializing the following JSON:

```json
{
  "field2": { "action":  "SET", "value":  "2" },
  "field3": { "action":  "SET", "value":  3 },
  "field4": { "action":  "NULL" }
}
```

In the case of INSERT, this will produce the statement:
 
 ```sql
INSERT INTO test (field1, field2, field3, field4) VALUES (DEFAULT, "2", 3, NULL)
```

Empty options are converted into DEFAULT, because this statement mandates that all columns are provided. 
Please note that in standard SQL, if you don't provide a column in an insert statement then its value will be DEFAULT. 
For example, suppose that you have a field "id" of type SERIAL inside postgres: if you create an insert statement 
without the column "id", then its insert value will be replace by DEFAULT, which in this case is equivalent to getting 
the sequence next value. In the case of a NULL constraint, the default is a NULL value. 
Note that using DEFAULT for a non nullable column will generate an error, unless that column was defined with a 
DEFAULT value inside the DDL.
Also note that fields inside an *EntityModifier* do not need to be aligned with the target entity; since we always 
specify all the columns and replace missing values with DEFAULT, the statement will produce a correct result.

In the case of UPDATE, this will produce the statement "UPDATE test SET field2 = "2", field3 = 3, field4 = NULL".
Empty options are skipped, but if you don't provide at least one defined *Option*, then the query will generate an
error, because the update statement mandates that at least one column is updated. It's perfectly fine to set a value
to NULL or DEFAULT, but using DEFAULT for a non nullable column will generate an error, unless that column was defined
with a DEFAULT value inside the DDL. Remember than you also need to provide an *EntityFilter* if you want to execute
an update statement.

Some people think that DEFAULT and NOT NULL are redundant: please read this answer on stack overflow to get an idea 
https://stackoverflow.com/questions/11862188/sql-column-definition-default-value-and-not-null-redundant.

## TableModifier

An *EntityModifier[MA]* can be used to produce an insert or update query on a *Table[A, K]* only if we have an implicit
instance of *TableModifier[A, MA]* in scope. The typeclass *TableModifier* guarantees that:

- an implicit *TableSyntax[A]* exists for *A*

- the modifier *MA* has all or a subset of the fields of *A*

- each *Modifier* is wrapped inside an *Option*

- the type of each *Modifier* field of *MA* is equivalent to the type of the corresponding field in *A*

These conditions are sufficient to generate a type-safe insert and update statements. To create a table modifier, use:

```scala
implicit val tableModifier: TableModifier[Test, TestModifier] = TableModifier.derive[Test, TestModifier]()
```

If the code compiles then you can be 100% sure that your modifier can be used with your entity.
