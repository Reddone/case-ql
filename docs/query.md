## SQL Action

Every SQL operation on Doobie produce either a *Connection[R]* or a fs2 *Stream[ConnectionIO, R]*, losing information
regarding the *Fragment* which produced such object. In order to keep the *Fragment*, these two types are replaced by
*SQLAction[R]* and *SQLStreamingAction[R]*.

They both have *toFragment* method which returns associated with the action, the former has an execute method which
returns a *ConnectionIO[R]* whereas the latter has an execute method which returns a *Stream[ConnectionIO, R]*. It is
possible to convert a *SQLStreamingAction* into a *SQLAction* by calling the *asSQLAction* method.

## TableQuery

*Table[T, K]* mixes the *TableQuery[T, K]* trait, which enables the creation of *SQLAction[R]* and 
*SQLStreamingAction[R]*. Anywhere in your code you can access the implicit instance of table using the convenience 
method apply. At this point, you have access to select, insert, update and delete methods:

```scala
val table = Table[T, K]

// SELECT

def select[FT <: EntityFilter[FT]](filter: FT, alias: Option[String])(
  implicit tableFilter: TableFilter[T, FT]
): SQLStreamingAction[T]

def selectByKey(key: K, alias: Option[String]): SQLAction[Option[T]]

// INSERT

def insert[MT <: EntityModifier[MT]](modifier: MT)(
  implicit tableModifier: TableModifier[T, MT]
): SQLAction[Int]

def insertReturningKey[MT <: EntityModifier[MT]](modifier: MT)(
  implicit tableModifier: TableModifier[T, MT]
): SQLAction[K]

// UPDATE

def update[MT <: EntityModifier[MT], FT <: EntityFilter[FT]](modifier: MT, filter: FT)(
  implicit tableModifier: TableModifier[T, MT], tableFilter: TableFilter[T, FT]
): SQLAction[Int]

def updateReturningKeys[MT <: EntityModifier[MT], FT <: EntityFilter[FT]](modifier: MT, filter: FT)(
  implicit tableModifier: TableModifier[T, MT], tableFilter: TableFilter[T, FT]
): SQLStreamingAction[K]

def updateByKey[MT <: EntityModifier[MT]](modifier: MT, key: K)(
  implicit tableModifier: TableModifier[T, MT]
): SQLAction[Int]

def updateByKeyReturningKeys[MT <: EntityModifier[MT]](modifier: MT, key: K)(
  implicit tableModifier: TableModifier[T, MT]
): SQLStreamingAction[K]

// DELETE

def delete[FT <: EntityFilter[FT]](filter: FT)(
  implicit tableFilter: TableFilter[T, FT]
): SQLAction[Int]

def deleteReturningKeys[FT <: EntityFilter[FT]](filter: FT)(
  implicit tableFilter: TableFilter[T, FT]
): SQLStreamingAction[K]

def deleteByKey(key: K): SQLAction[Int]

def deleteByKeyReturningKeys(key: K): SQLStreamingAction[K]
```

## QueryBuilder

*TableQuery[T, K]* is backed by a set of classes extending *QueryBuilder[T, K]*. There is a builder for select, insert, 
update and delete operations. The good thing is that builders are type-safe, i.e. you can call a builder method only
if you have previously called another builder method; the compiler will stop you either if you try to call the build
method directly or if you try to call of method in the chain without calling its predecessors. Here's an example:

```scala
val updateBuilder = UpdateBuilder
  .forTable(table)          // initialize builder with explicit table
  .withModifier(modifier)   // you can add a modifier only to a fresh builder
  .withFilter(filter)       // you can add a filter only if you added a modifier
  .buildUpdateReturningKeys // you can build only if you added a modifier and a filter

val deleteBuilder = DeleteBuilder[T, K] // initialize builder with implicit table
 .withFilter(filter)                    // you can add a filter only to a fresh builder
 .buildDelete                           // you can build only if you added a filter
```

Note that these builders are backed by fluent API, i.e. they return the same builder object instead of creating 
a new builder each time a method is invoked.
