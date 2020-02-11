## Table

A Table[T, K] is a typeclass which evidences that T can be correctly read and written in SQL and that K is a subset
of T in the sense that it has a subset of fields having same types. To create a table, simply use:

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

val table: Table[Test, TestKey] = Table.derive[Test, TestKey]()
```

The table derivation method can be used with its defaults or with user overrides. It is possible to customize the table
using the following arguments:

```scala
val name = Some("test_name")
val schema = Some("test_schema")
val fieldConverter = Map("field1" -> "field_1", "field2" -> "field_2")
val fieldMapper = (str: String) => str.toUpperCase

val table: Table[Test, TestKey] = Table.derive[Test, TestKey](name, schema, fieldConverter, fieldMapper)
```

The defaults are to use the TypeTag typeSymbol in snake case for the name, None for the schema, an empty Map for the
field converter and camel to snake case conversion for the mapper. The field converter always has higher precedence
than the field mapper, so if you want to fully customize column names you can provide just a field converter.

Please note that this part of code it's not typesafe. This library ensure type safety at the case class level, the user
must provide a valid mapping to table column names (many JDBC libraries leave this responsibility to the user). However,
type K is checked against T using shapeless record Extractor.

Once you have a table, you can access its syntax:

```scala
val defaultSyntax: table.Syntax = table.defaultSyntax
val syntax: table.Syntax        = table.syntax("t")

defaultSyntax.name             // "test_schema.test_name"
defaultSyntax.columns          // List("field_1", "field_2", "FIELD3", "FIELD4")
defaultSyntax.column("field1") // "field_1"
defaultSyntax.field1           // "field_1"
defaultSyntax.field3           // "FIELD3"

syntax.name             // "test_schema.test_name t"
syntax.columns          // List("t.field_1", "t.field_2", "t.FIELD3", "t.FIELD4")
syntax.column("field1") // "t.field_1"
syntax.field1           // "t.field_1"
syntax.field3           // "t.FIELD3"
```

Syntax is a facility which provides access to table information: you should not use the table object directly because,
for example, it doesn't add the schema to the table name nor it prefixes columns with the table alias.

Finally, a table holds the implicit instances for Read and Write, in order to use them when performing queries.
