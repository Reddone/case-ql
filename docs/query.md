## SQL Action

Every SQL operation on Doobie produce either a *Connection[R]* or a fs2 *Stream[ConnectionIO, R]*, losing information
regarding the *Fragment* which produced such object. In order to keep the *Fragment*, these two types are replaced by
*SQLAction[R]* and *SQLStreamingAction[R]*.

They both have *toFragment* method which returns associated with the action, the former has an execute method which
returns a *ConnectionIO[R]* whereas the latter has an execute method which returns a *Stream[ConnectionIO, R]*. It is
possible to convert a *SQLStreamingAction* into a *SQLAction* by calling the *asSQLAction* method.

## TableQuery

*Table[A, K]* mixes the *TableQuery[A, K]* trait, which enables the creation of *SQLAction[R]* and 
*SQLStreamingAction[R]*. Anywhere in your code you can access the implicit instance of table using the convenience 
method apply. At this point, you have access to select, insert, update and delete methods:

```scala
// summon implicit instance already available in scope

val table = Table[A, K] 

// SELECT

def select[FA <: EntityFilter[FA]](filter: FA, alias: Option[String])(
  implicit tableFilter: TableFilter[A, FA]
): SQLStreamingAction[A]

def selectByKey(key: K, alias: Option[String]): SQLAction[Option[A]]

// INSERT

def insert[MA <: EntityModifier[MA]](modifier: MA)(
  implicit tableModifier: TableModifier[A, MA]
): SQLAction[Int]

def insertReturningKey[MA <: EntityModifier[MA]](modifier: MA)(
  implicit tableModifier: TableModifier[A, MA]
): SQLAction[K]

// UPDATE

def update[MA <: EntityModifier[MA], FA <: EntityFilter[FA]](modifier: MA, filter: FA)(
  implicit tableModifier: TableModifier[A, MA], tableFilter: TableFilter[A, FA]
): SQLAction[Int]

def updateReturningKeys[MA <: EntityModifier[MA], FA <: EntityFilter[FA]](modifier: MA, filter: FA)(
  implicit tableModifier: TableModifier[A, MA], tableFilter: TableFilter[A, FA]
): SQLStreamingAction[K]

def updateByKey[MA <: EntityModifier[MA]](modifier: MA, key: K)(
  implicit tableModifier: TableModifier[A, MA]
): SQLAction[Int]

def updateByKeyReturningKeys[MA <: EntityModifier[MA]](modifier: MA, key: K)(
  implicit tableModifier: TableModifier[A, MA]
): SQLStreamingAction[K]

// DELETE

def delete[FA <: EntityFilter[FA]](filter: FA)(
  implicit tableFilter: TableFilter[A, FA]
): SQLAction[Int]

def deleteReturningKeys[FA <: EntityFilter[FA]](filter: FA)(
  implicit tableFilter: TableFilter[A, FA]
): SQLStreamingAction[K]

def deleteByKey(key: K): SQLAction[Int]

def deleteByKeyReturningKeys(key: K): SQLStreamingAction[K]
```

## QueryBuilder

*TableQuery[A, K]* is backed by a set of classes extending *QueryBuilder[A, K]*. There is a builder for select, insert, 
update and delete operations. The good thing is that builders are type-safe, i.e. you can call a builder method only
if you have previously called another builder method; the compiler will stop you either if you try to call the build
method directly or if you try to call of method in the chain without calling its predecessors. Here's an example:

```scala
val updateBuilder = UpdateBuilder
  .forTable(table)          // initialize builder with explicit table
  .withModifier(modifier)   // you can add a modifier only to a fresh builder
  .withFilter(filter)       // you can add a filter only if you added a modifier
  .buildUpdateReturningKeys // you can build only if you added a modifier and a filter

val deleteBuilder = DeleteBuilder[A, K] // initialize builder with implicit table
 .withFilter(filter)                    // you can add a filter only to a fresh builder
 .buildDelete                           // you can build only if you added a filter
```

Note that these builders are backed by fluent API, i.e. they return the same builder object instead of creating 
a new builder each time a method is invoked. The plan is to keep them as a private api, so if you want to issue a 
query use the *Table* instance instead.
