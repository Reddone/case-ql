## Table

A Table[T, K] is a wrapper for a type T having a key K. Both T and K are case classes. Scala fields are converted to
SQL columns using a camel case to snake case converter; if you want to override the mapping, you can a custom mapper
in the form of a Map[String, String], where each entry is in the form (scala field -> sql column).

[Table documentation](./docs/table.md)

## Filter

A Filter[+A] is a wrapper for a type A on which it's possible to express filter expressions. The library provides
instances of various Filter, for A and Option[A]: they only differ in the is nullable condition. For example, on an
IntFilter you can use the following expressions: EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE.
You can create filters on case classes using the FilterWrapper trait and assigning to each field a Filter[_]. Then,
you can create a TableFilter[T, U] on the target type T, meaning that U can be used to generate a "WHERE" on the
table represented by T.

<!-- [Filter documentation](./docs/filter.md) -->

## Modifier

A Modifier[+A] is a wrapper for a type A on which it's possible to set values. The library provides instances of
various Modifier, for A and Option[A]: they only differ in the possibility to set a "NULL" value. For example, on an
IntModifier you can either set "DEFAULT" or a value of type Int.
You can create modifiers on case classes assigning to each field a Modifier[_]. Then, you can create a
TableModifier[T, U] on the target type T, meaning that U can be used to generate "INSERT" and "UPDATE" on the table
represented by T.

<!-- [Modifier documentation](./docs/modifier.md) -->

## Query

Once you have a Table[T, K], you can execute elementary SQL queries. At the moment, these are the supported ones:
- "SELECT" * using a custom TableFilter.
- "INSERT" using a custom TableModifier.
- "UPDATE" using a custom TableModifier and TableFilter.
- "DELETE" using a custom TableFilter.

<!-- [Query documentation](./docs/modifier.md) -->

## Util

There are some interesting utilities, which can be useful depending on the scenario:
- possibility to work with raw values in the form of a Map[String, Any]. Maybe you don't want to provide a Read or 
Write instance but you want to read data as it is. You can do this using the Read[Row] and Write[Row] implicit
instances inside the Raw object. For example, if you have a huge table and you want to use Sangria Projection, now
you can. Use it at your own risk because it remove the type safe layer introduced by doobie.
- test utilities which can save you some time. There is a GenericRepository which exposes common operations in an
unsafe manner, but it removes many of the test boilerplate, and there is a TestTransactor factory to create a
Transactor backed by different pools.
- some fragment utils, like the optionalAndOpt and optionalOrOpt which are super handy when building dynamic SQL
- various helpers in the form of StringUtils, JsonUtils, CirceDecoders, ExecutorServices.
- a FromMap typeclass which can be used in conjunction with Row to convert raw results back to case classes.
For example, you can transform the raw result according to a Sangria Projector and then you can map your intermediate
result to a case class before the final mapping step with the GraphQL object.
- SQL tokens and functions used inside the project to avoid repetitions. Maybe you can use them too.

<!-- [Util documentation](./docs/util.md) -->