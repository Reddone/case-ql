## Table

A *Table[T, K]* is a typeclass which evidences that *T* can be correctly read and written in SQL and that *K* is a 
subset of *T* in the sense that it has a subset of fields having same types. To create a table, simply use:

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

implicit val testTable: Table[Test, TestKey] = Table.derive[Test, TestKey]()
```

The table derivation method can be used with its defaults or with user overrides. It is possible to customize the table
using the following arguments:

```scala
val name           = Some("test_name")
val schema         = Some("test_schema")
val fieldConverter = Map("field1" -> "field_1", "field2" -> "field_2")
val fieldMapper    = (str: String) => str.toUpperCase
val useTableAlias  = true

implicit val testTable: Table[Test, TestKey] = 
  Table.derive[Test, TestKey](name, schema, fieldConverter, fieldMapper, useTableAlias)
```

The defaults are to use the *TypeTag* typeSymbol in snake case for the name, *None* for the schema, an empty *Map* for 
the field converter and camel to snake case conversion for the mapper. The field converter always has higher precedence
than the field mapper, so if you want to fully customize column names you can provide just a field converter.

Please note that this part of code it's not typesafe. This library ensure type safety at the case class level, the user
must provide a valid mapping to table column names (many JDBC libraries leave this responsibility to the user). However,
type *K* is checked against *T* using shapeless record *Extractor*.

The last argument is used to enable unique aliases in the form "xN" where "x" is a constant prefix and "N" is a
counter. It defaults to true, but if you can disable it.

A *Table[T, K]* also holds implicits instances for *Read[T]*, *Write[T]*, *Read[K]*, *Write[K]*: it proves that *T* 
and *K* can be safely used with JDBC.

## TableSyntax

A *TableSyntax[T]* is a facility class for decoupling *T* from *K* and for accessing and aliasing table columns during a
query. If you have an implicit instance of *Table[T, K]*, then an implicit instance of *TableSyntax[T]* is automatically 
derived. You can also access the syntax directly from table: 

```scala
implicit val testTable: Table[Test, TestKey] = Table.derive[Test, TestKey](Some("test"), Some("public"))

val syntax: TableSyntax[T]        = testTable.syntax
val aliasedSyntax: TableSyntax[T] = testTable.syntax.withAlias(Some("t"))

syntax.name             // "public.test"
syntax.aliasedName      // "public.test"
syntax.columns          // List("field1", "field2", "field3", "field4")
syntax.column("field1") // "field1"
syntax.field1           // "field1"

aliasedSyntax.name             // "public.test"
aliasedSyntax.aliasedName      // "public.test t"
aliasedSyntax.columns          // List("t.field1", "t.field2", "t.field3", "t.field4")
aliasedSyntax.column("field1") // "t.field1"
aliasedSyntax.field1           // "t.field1"
```

You never interact directly with this class, because it is used only during the derivation process of other typeclasses:
however, since you can provide an alias inside many queries, syntax is an easy way to encapsulate what changes if you
want to use a custom alias.

## TableLink

A *TableLink[A, B]* is an auxiliary class which is used to signal that *A* and *B* participate in a relation.
There are three kinds of link:

- self link, in the form *TableLink[A, A]*. It indicates that *A* has one or more fields which point to *A* itself. 

- direct link, in the form *TableLink[A, B]*. It indicates that *A* and *B* are connected by one or more fields.

- junction link signals that *A* and *B* are linked together via a junction table *C*. The type *C* is encoded inside 
the *TableLink* class and you can use *TableLink.Aux[A, B, C]* to refer to it.

To create a link, you can use the following:

```scala
case class TestDirect(
  field1: String,
  field2: Option[Long],
  field3: Timestamp,
  testField1: Int
)
case class TestDirectKey(
  field: String
)

implicit val testDirectTable    = Table.derive[TestDirect, TestDirectKey]()

implicit val testTestDirectLink = TableLink.direct(testTable, testDirectTable)(
  (a, b) => NonEmptyList.of(("field1", "testField1"))
)

case class TestLeft(
  leftField1: Long,
  leftField2: String,
  leftField3: Int
)
case class TestLeftKey(
  leftField1: Long
)

case class TestMiddle(
  middleField1: Long,
  middleField2: Int
)
case class TestMiddleKey(
  middleField1: Long,
  middleField2: Int
)

case class TestRight(
  rightField1: Int,
  rightField2: Long,
  rightField3: String
)
case class TestRightKey(
  rightField1: Int
)

implicit val testLeftTable     = Table.derive[TestLeft, TestLeftKey]()
implicit val testMiddleTable   = Table.derive[TestMiddle, TestMiddleKey]()
implicit val testRightTable    = Table.derive[TestRight, TestRightKey]()

implicit val testTestRightLink = TableLink.junction(testLeftTable, testRightTable, testMiddleTable)( 
  (a, c) => NonEmptyList.of(("leftField1", "middleField1")),
  (b, c) => NonEmptyList.of(("rightField1", "middleField2"))
)
```

Thanks to implicit derivation, there is no need to define the inverse link, i.e. if you have an implicit instance of
*TableLink[A, B]* you don't have to define *TableLink[B, A]*.
*TableLink* is currently experimental but it will be fixed and made type-safe soon.
