## Introduction

Here we report the basic concepts behind CaseQL. For an in depth explanation, please follow the documentation links
at the end of each section. If you have any question which doesn't find an answer in the documentation, feel free
to open an issue or to write an e-mail.

Before reading the documentation, I suggest you to become familiar with Doobie, by reading the "book of doobie" at
https://tpolecat.github.io/doobie/. Since at the moment this library is heavily based on it, it's better for you
to become familiar with concepts such as *Read*, *Write*, *Query*, *Update* and *Transactor*. There are also nice 
examples at https://github.com/tpolecat/doobie/tree/master/modules/example/src/main/scala/example.

## SQL Doobie Module

### Table

A *Table[T, K]* is a wrapper for a type *T* having a key *K*. Both *T* and *K* are case classes. Scala fields are 
converted to SQL columns using a camel case to snake case converter; if you want to override the mapping, you can 
provide a custom mapper in the form of a *Map[String, String]*, where each entry is in the form 
(Scala field -> SQL table column). In order to make a case class *T* work with this library, you need to create an 
implicit instance of *Table* using the appropriate derivation method.

Tables can be linked together using the *Link[A, B]* class. If you want to use relations, you have to provide and
implicit *Link* instance using one of the link factory methods. This needs to be done only once, i.e. if you have a
*Link[A, B]* you don't have to define a *Link[B, A]*.

Basically, tables and links are the entry point for other concepts. They are used at compile time to enforce the 
structure of *Filter* and *Modifier*, meaning that you won't be allowed to compile the code if you want to use a non
compliant filter to query a table. 

[Table documentation](./table.md)

### Filter

A *Filter[+A]* is a wrapper for a type *A* on which it's possible to express filter expressions. The library provides
instances of various *Filter*, for *A* and *Option[A]*: they only differ in the is nullable condition. For example, 
on an *IntFilter* you can use the following expressions: EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE.

You can create filters on case classes by mixing the *EntityFilter* trait and assigning to each field a *Filter[_]* or 
a *RelationFilter[_, _, _]*. Then, you need to create an implicit instance of *TableFilter[T, F]* using the derivation 
method: if the code compiles, it means that *F* can be used to generate a where condition on a table having type *T*.

You can build complex filters using AND, OR and NOT conditions on an entity and using EVERY, SOME, NONE conditions
on its relations. The implicit *TableFilter[T, F]* will take care of combining all these conditions, producing an
efficient SQL query. Filters can be used inside select, update and delete statements.

[Filter documentation](./filter.md)

### Modifier

A *Modifier[+A]* is a wrapper for a type *A* on which it's possible to set values. The library provides instances of
various *Modifier*, for *A* and *Option[A]*: they only differ in the possibility to set a NULL value. For example, 
on an *IntModifier* you can either set DEFAULT or a value of type *Int* and on a *StringModifierOption* you can set 
DEFAULT, NULL or a value of type *String*.

You can create modifiers on case classes by mixing the *EntityModifier* trait and assigning to each field a 
*Modifier[_]*. Then, you need to create an implicit instance of *TableModifier[T, M]* using the derivation method: 
if the code compiles, it means that *M* can be used to set values on a table having type *T*.

Note that we don't care about auto generated keys or fields not having default values: these parts are better managed
by SQL DDL and in my opinion there is no need to introduce such concepts in a library like this. The use case of
performing multiple inserts or updates is left to the user.

[Modifier documentation](./modifier.md)

### Query

Once you have a *Table[T, K]*, you can execute elementary SQL queries. At the moment, these are the supported ones:

- SELECT * using a *TableFilter[T, F]* with filter *F*, returning *Stream[ConnectionIO, T]*.

- SELECT * using key *K*, returning *ConnectionIO[Option[T]]*.

- INSERT using a *TableModifier[T, M]* with modifier *M*, returning *ConnectionIO[Int]*.

- INSERT RETURNING using a *TableModifier[T, M]* with modifier *M* and returning *ConnectionIO[K]*.

- UPDATE using a *TableModifier[T, M]* with modifier *M* and a *TableFilter[T, F]* with filter *F*, 
returning *ConnectionIO[Int]*.

- UPDATE RETURNING using a *TableModifier[T, M]* with modifier *M* and a *TableFilter[T, F]* with filter *F*, 
returning *Stream[ConnectionIO, K]*.

- UPDATE using a *TableModifier[T, M]* with modifier *M* and key *K*, returning *ConnectionIO[Int]*.

- UPDATE RETURNING using a *TableModifier[T, M]* with modifier *M* and a key *K*, returning *Stream[ConnectionIO, K]*.

- DELETE using a *TableFilter[T, F]* with filter *F*, returning *ConnectionIO[Int]*.

- DELETE RETURNING using a *TableFilter[T, F]* with filter *F*, returning *Stream[ConnectionIO, K]*.

- DELETE using key *K*, returning *ConnectionIO[Int]*.

- DELETE RETURNING using key *K*, returning *Stream[ConnectionIO, K]*.

At the moment joins are not supported, but since we have links I think that it will be possible to express type-safe
joins in the future. If you need joins, you can use the generated fragment and enrich it as you like.

[Query documentation](./modifier.md)

### Util

There are some interesting utilities, which can be useful depending on the scenario:

- possibility to work with raw values in the form of a *Map[String, Any]*. Maybe you don't want to provide a *Read* or 
*Write* instance but you want to read data as it is. You can do this using the *Read[Row]* and *Write[Row]* implicit
instances inside the *Raw* object. For example, if you have a huge table and you want to use Sangria projections, now
you can. Use it at your own risk because they remove the type safe layer introduced by doobie.

- test utilities which can save you some time. There is a *GenericRepository* which exposes common operations in an
unsafe manner, but it removes many of the test boilerplate, and there is a *TestTransactors* factory to create a
Doobie transactor backed by different pools.

- some fragment utils, like the *optionalAndOpt* and *optionalOrOpt* which are super handy when building dynamic SQL.

- various helpers in the form of *StringUtils*, *JsonUtils*, *CirceDecoders*, *ExecutorServices*.

- a *FromMap* typeclass which can be used in conjunction with *Row* to convert raw results back to case classes.
For example, you can transform the raw result according to a *Projector* and then you can map your intermediate
result to a case class before the final mapping step with the GraphQL object.

- SQL tokens and functions used inside the project to avoid repetitions. Maybe you can use them too.

[Util documentation](./util.md)

## GraphQL Sangria Module

### Scalar

### Input

### Util
