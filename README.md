# Case QL

Small library providing basic CRUD operations for SQL backend when using GraphQL. At the moment it only supports 
[doobie](https://github.com/tpolecat/doobie) for SQL and [sangria](https://github.com/sangria-graphql/sangria) 
for GraphQL, but I am planning to add support for [scalikejdbc](https://github.com/scalikejdbc/scalikejdbc) and 
[caliban](https://github.com/ghostdogpr/caliban). I am not planning to add [slick](https://github.com/slick/slick) and 
[quill](https://github.com/getquill/quill) support because they are not based or they not provide support for the 
fragment approach (I strongly believe that we should write SQL and not try to port SQL inside Scala).

It leverages scala case classes and shapeless to build type safe SQL queries from a GraphQL Input. The project was 
born because I was tired of writing boilerplate for SQL, especially complex filters. Since GraphQL queries are 
"client-driven" the most time consuming task was to write all the kind of possible filters for entities; this problem 
was also addressed by [prisma](https://www.prisma.io/docs/reference/prisma-api/queries-ahwee4zaey#filtering-by-field), 
but honestly even if the project is very interesting I'd like to stick with Scala and custom code, especially if some 
queries require SQL ad-hoc customization. Actually there is no support for relations, I am open to discussions on how 
we can add them.

See the "example" sub project to see it in action! It is still an experiment but it helped me a lot!!!
I am writing examples along with tests and documentation these month, so please
wait a little. If you want you can explore the source code, the core is located
inside the "generic" package of the "sql" module.

## Introduction

WORK IN PROGRESS FOR DETAILED DOCUMENTATION

## Table

A Table[T, K] is a wrapper for a type T having a key K. Both T and K are case classes. Scala fields are converted to
SQL columns using a camel case to snake case converter; if you want to override the mapping, you can a custom mapper
in the form of a Map[String, String], where each entry is in the form (scala field -> sql column).

<!-- [here](./docs/table.md) -->

## Filter

A Filter[+A] is a wrapper for a type A on which it's possible to express filter expressions. The library provides
instances of various Filter, for A and Option[A]: they only differ in the is nullable condition. For example, on an
IntFilter you can use the following expressions: EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE.
You can create filters on case classes using the FilterWrapper trait and assigning to each field a Filter[_]. Then,
you can create a TableFilter[T, U] on the target type T, meaning that U can be used to generate a "WHERE" on the
table represented by T.

<!-- [here](./docs/filter.md) -->

## Modifier

A Modifier[+A] is a wrapper for a type A on which it's possible to set values. The library provides instances of
various Modifier, for A and Option[A]: they only differ in the possibility to set a "NULL" value. For example, on an
IntModifier you can either set "DEFAULT" or a value of type Int.
You can create modifiers on case classes assigning to each field a Modifier[_]. Then, you can create a
TableModifier[T, U] on the target type T, meaning that U can be used to generate "INSERT" and "UPDATE" on the table
represented by T.

<!-- [here](./docs/modifier.md) -->

## Query

Once you have a Table[T, K], you can execute elementary SQL queries. At the moment, these are the supported ones:
- "SELECT" * using a custom TableFilter.
- "INSERT" using a custom TableModifier.
- "UPDATE" using a custom TableModifier and TableFilter.
- "DELETE" using a custom TableFilter.

<!-- [here](./docs/modifier.md) -->

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

<!-- [here](./docs/util.md) -->

## TODO

- setup an easy build and release process and move to sonatype
- add a combinator option for both the entire filter and each singular field (actually it defaults to AND) 
- abstract over Fragment in order to include scalalikejdbc support
- provide support for caliban (I still have to study the project)
- add relations (one to one, one to many, many to many)
- more examples
- fix spacing inside queries (not necessary but nice to have)
- add a logo if I get 10 stars
