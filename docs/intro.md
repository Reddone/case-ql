## Introduction

Here we report the basic concepts behind Case-QL. For an in depth explanation, please follow the documentation links
at the end of each section. If you have any question for which you can't find an answer in the documentation, feel free
to open an issue or to write an e-mail.

Before reading the documentation, I suggest you to become familiar with Doobie, by reading the "book of doobie" at
https://tpolecat.github.io/doobie/. Since this library is heavily based on it, it's better for you to become familiar 
with concepts such as *Read*, *Write*, *Query*, *Update* and *Transactor*. There are also nice examples at 
https://github.com/tpolecat/doobie/tree/master/modules/example/src/main/scala/example.

## SQL Doobie Module

### Table

A *Table[A, K]* is a wrapper for a type *A* having a key *K*. Both *A* and *K* are case classes. Scala fields are 
converted to SQL columns using a camel case to snake case converter; if you want to override the mapping, you can 
provide a custom mapper in the form of a *Map[String, String]*, where each entry is in the form 
(Scala field -> SQL table column). In order to make a case class *A* work with this library, you need to create an 
implicit instance of *Table* using the appropriate derivation method.

Tables can be linked together using the *TableLink[A, B]* class. If you want to use relations, you have to provide and
implicit *TableLink* instance using one of the link factory methods. This needs to be done only once, i.e. if you have 
a *TableLink[A, B]* you don't have to define a *TableLink[B, A]*.

Basically, tables and links are the entry point for other concepts. They are used at compile time to enforce the 
structure of *Filter* and *Modifier*, meaning that you won't be allowed to compile the code if you want to use a non
compliant filter to query a table. 

[Table documentation](./table.md)

### Filter

A *Filter[T]* is a wrapper for a type *T* on which it's possible to express filter expressions. This library provides
instances of various *Filter*, for *T* and *Option[T]*: they only differ in the is nullable condition. For example, 
on an *IntFilter* you can use the following expressions: EQ, NOT_EQ, IN, NOT_IN, LT, LTE, GT, GTE.

You can use a case class *F* as a filter by mixing the *EntityFilter* trait and assigning to each field a 
*Filter[_]* or a *RelationFilter[_, _, _]*. Then, you need to create an implicit instance of *TableFilter[A, F]* using 
the derivation method: if the code compiles, it means that case class *F* can be used to generate a where condition on 
a table having type *A*.

You can build complex filters using AND, OR and NOT conditions on an entity and using EVERY, SOME, NONE conditions
on its relations. The implicit *TableFilter[A, F]* will take care of combining all these conditions, producing an
efficient SQL query. Filters can be used inside select, update and delete statements.

[Filter documentation](./filter.md)

### Modifier

A *Modifier[T]* is a wrapper for a type *T* on which it's possible to set values. This library provides instances of
various *Modifier*, for *T* and *Option[T]*: they only differ in the possibility to set a NULL value. For example, 
on an *IntModifier* you can either set DEFAULT or a value of type *Int* and on a *StringModifierOption* you can set 
DEFAULT, NULL or a value of type *String*.

You can use a case class *M* as a modifier by mixing the *EntityModifier* trait and assigning to each field a 
*Modifier[_]*. Then, you need to create an implicit instance of *TableModifier[A, M]* using the derivation method: 
if the code compiles, it means that *M* can be used to set values on a table having type *A*.

Note that we don't care about auto generated keys or fields not having default values: these parts are better managed
by SQL DDL and in my opinion there is no need to introduce such concepts in a library like this. The use case of
performing multiple inserts or updates is left to the user.

[Modifier documentation](./modifier.md)

### Query

Once you have a *Table[A, K]*, you can execute elementary SQL queries. At the moment, these are the supported ones:

- SELECT * using a *TableFilter[A, F]* with filter *F*, returning *Stream[ConnectionIO, A]*.

- SELECT * using key *K*, returning *ConnectionIO[Option[A]]*.

- INSERT using a *TableModifier[A, M]* with modifier *M*, returning *ConnectionIO[Int]*.

- INSERT RETURNING using a *TableModifier[A, M]* with modifier *M* and returning *ConnectionIO[K]*.

- UPDATE using a *TableModifier[A, M]* with modifier *M* and a *TableFilter[A, F]* with filter *F*, 
returning *ConnectionIO[Int]*.

- UPDATE RETURNING using a *TableModifier[A, M]* with modifier *M* and a *TableFilter[A, F]* with filter *F*, 
returning *Stream[ConnectionIO, K]*.

- UPDATE using a *TableModifier[A, M]* with modifier *M* and key *K*, returning *ConnectionIO[Int]*.

- UPDATE RETURNING using a *TableModifier[A, M]* with modifier *M* and a key *K*, returning *Stream[ConnectionIO, K]*.

- DELETE using a *TableFilter[A, F]* with filter *F*, returning *ConnectionIO[Int]*.

- DELETE RETURNING using a *TableFilter[A, F]* with filter *F*, returning *Stream[ConnectionIO, K]*.

- DELETE using key *K*, returning *ConnectionIO[Int]*.

- DELETE RETURNING using key *K*, returning *Stream[ConnectionIO, K]*.

At the moment joins are not supported, but since we have links I think that it will be possible to express type-safe
joins in the future. If you need joins, you can use the generated fragment and enrich it as you like.

[Query documentation](./modifier.md)

### Util

There are some interesting utilities, which can be useful depending on the scenario:

- possibility to work with raw values in the form of a *Map[String, Any]*. Maybe you don't want to provide a *Read* or 
*Write* instance, but you want to read data as it is. You can do this using the *Read[Row]* and *Write[Row]* implicit
instances inside the *Raw* object. For example, if you have a huge table, and you want to use Sangria projections, now
you can. Use it at your own risk because they remove the type safe layer introduced by Doobie.

- test utilities which can save you some time. There is a *GenericRepository* which exposes common operations in an
unsafe manner, but it removes many of the test boilerplate, and there is a *TestTransactors* factory to create a
Doobie transactor backed by different thread pools.

- some fragment utils, like the *optionalAndOpt* and *optionalOrOpt* which are super handy when building dynamic SQL.

- a *FromMap* typeclass which can be used in conjunction with *Row* to convert raw results back to case classes.
For example, you can transform the raw result according to a *Projector* and then you can map your intermediate
result to a case class before the final mapping step with the GraphQL object.

- SQL tokens and functions used internally in the project to avoid repetitions. Maybe you can use them too.

[Util documentation](./util.md)

## JSON Circe Module 

This module contains circe decoders for *Filter*, *Modifier* and other types not covered by Circe.

## GraphQL Sangria Module

### Scalar

For every type used inside *Filter* and *Modifier*, there is a corresponding implicit *ScalarType*. 
You can find these instances inside *ByteTypeDefinition*, *JavaSqlTypeDefinition* and *JavaTimeTypeDefinition*.

### Input

For every *Filter* and *Modifier* provided by this library, there is a corresponding implicit *InputObjectType*.
You can find these instances inside *InputTypeDefinition*. There is also a utility method *makeRelationFilterInputType*
which you can use to create an *InputObjectType* for a *RelationFilter*.
