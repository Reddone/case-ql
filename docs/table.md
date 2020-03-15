## Table

A *Table[A, K]* is a typeclass which evidences that *A* can be correctly read and written in SQL and that *K* is a 
subset of *A* in the sense that it has a subset of fields having same types. To create a table, simply use:

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
type *K* is checked against *A* using shapeless record *Extractor* to ensure that the table has the key fields.

The last argument is used to enable unique aliases in the form "xN" where "x" is a constant prefix and "N" is a
counter. It defaults to true, but if you can disable it.

A *Table[A, K]* also holds implicits instances for *Read[A]*, *Write[A]*, *Read[K]*, *Write[K]*: it proves that *A* 
and *K* can be safely used with JDBC.

## TableSyntax

A *TableSyntax[A]* is a facility class for decoupling *A* from *K* and for accessing and aliasing table columns during 
a query. If you have an implicit instance of *Table[A, K]*, then an implicit instance of *TableSyntax[A]* is 
automatically derived. You can also access the syntax directly from table: 

```scala
val testTable: Table[Test, TestKey] = Table.derive[Test, TestKey](Some("test"), Some("public"), useTableAlias = false)
val syntax: TableSyntax[T]          = testTable.syntax
val aliasedSyntax: TableSyntax[T]   = testTable.syntax.withAlias(Some("t"))

syntax.name                    // "public.test"
syntax.aliasedName             // "public.test"
syntax.columns                 // List("field1", "field2", "field3", "field4")
syntax.aliasedColumns          // List("test.field1", "test.field2", "test.field3", "test.field4")
syntax.column("field1")        // "field1"
syntax.aliasedColumn("field1") // "test.field1"

aliasedSyntax.name                    // "public.test"
aliasedSyntax.aliasedName             // "public.test t"
aliasedSyntax.columns                 // List("field1", "field2", "field3", "field4")
aliasedSyntax.aliasedColumns          // List("t.field1", "t.field2", "t.field3", "t.field4")
aliasedSyntax.column("field1")        // "field1"
aliasedSyntax.aliasedColumn("field1") // "t.field1"
```

You never interact directly with this class, because it is used only during the derivation process of other typeclasses:
however, since you can provide an alias inside many queries, syntax is an easy way to encapsulate what changes if you
want to use a custom alias.

## TableLink

A *TableLink[A, B]* is a typeclass which evidences that *A* and *B* have some fields in common having the same types,
thus it's possible to use these fields to describe a relation. To be more precise, we deal with the the auxiliary
type *TableLink.Aux[A, B, C]* where *C* is *Unit* in the case of a relation between two tables and *C* is a case
class in the case of a relation between two tables where a junction table is involved. 

There are three kinds of link:

- self link, in the form *TableLink[A, A]*. It indicates that *A* has one or more fields which point to *A* itself. 

- direct link, in the form *TableLink[A, B]*. It indicates that *A* and *B* are connected by one or more fields.

- junction link, which signals that *A* and *B* are linked together via a junction table *C*. The type *C* is encoded 
inside the *TableLink* class and you can use *TableLink.Aux[A, B, C]* to refer to it.

Suppose you have the following tables:

```scala
// left, having a self relation and a junction relation with right
case class TestLeft(
    field1: Int,
    field2: String,
    field3: Int
)
case class TestLeftKey(
    field1: Int
)
// direct, having a direct relation with left
case class TestDirect(
    field1: String,
    field2: Timestamp,
    field3: Int
)
case class TestDirectKey(
    field1: String
)
// right, having a junction relation with left
case class TestRight(
    field1: Long,
    field2: String,
    field3: Int
)
case class TestRightKey(
    field1: Long
)
// junction table between left and right
case class TestJunction(
    field1: Int,
    field2: Long
)
case class TestJunctionKey(
    field1: Int,
    field2: Long
)

implicit val leftTable: Table[TestLeft, TestLeftKey] =
  Table.derive[TestLeft, TestLeftKey](useTableAlias = false)

implicit val directTable: Table[TestDirect, TestDirectKey] =
  Table.derive[TestDirect, TestDirectKey](useTableAlias = false)

implicit val rightTable: Table[TestRight, TestRightKey] =
  Table.derive[TestRight, TestRightKey](useTableAlias = false)

implicit val junctionTable: Table[TestJunction, TestJunctionKey] =
  Table.derive[TestJunction, TestJunctionKey](useTableAlias = false)
```

To create links between tables, you can use the following:

```scala
implicit val leftSelfLink: Aux[TestLeft, TestLeft, Unit] = TableLink.self[TestLeft](
  FieldSet("field1"),
  FieldSet("field3")
)

implicit val directLeftLink: Aux[TestDirect, TestLeft, Unit] = TableLink.direct[TestDirect, TestLeft](
  FieldSet("field3"),
  FieldSet("field1")
)

implicit val leftJunctionLink: Aux[TestLeft, TestJunction, Unit] = TableLink.direct[TestLeft, TestJunction](
  FieldSet("field1"),
  FieldSet("field1")
)

implicit val rightJunctionLink: Aux[TestRight, TestJunction, Unit] = TableLink.direct[TestRight, TestJunction](
  FieldSet("field1"),
  FieldSet("field2")
)

implicit val leftRightLink: Aux[TestLeft, TestRight, TestJunction] = TableLink.union(
  leftJunctionLink, 
  rightJunctionLink
)
```

There is also a *TableLink.junction* factory method, which you can use if you want to create a link without using
intermediate direct links with the junction table:

```scala
implicit val leftRightLink: Aux[TestLeft, TestRight, TestJunction] = 
  TableLink.junction[TestLeft, TestRight, TestJunction](
    (FieldSet("field1"), FieldSet("field1")),
    (FieldSet("field1"), FieldSet("field2"))
  )
```

Thanks to implicit derivation, there is no need to define the inverse link, i.e. if you have an implicit instance of
*TableLink[A, B]* you don't have to define *TableLink[B, A]*.
