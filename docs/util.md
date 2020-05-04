## Raw

Sometimes you need to deal with raw SQL data or simply you don't want to give a query a specific type *T* for building
its return type. The first example that comes to mind are projections: when you want to load only some fields of a
table, you have to either to build a tuple containing your columns or provide a case class or a tuple of case classes.
Using the *Raw* util, you can read and write raw values in the form *Map[String, Any]*.
To be more precise, a *mutable.LinkedHashMap* is used since insertion order matters when you are filling prepared 
statement parameters:

```scala
import Raw._

type Row = mutable.LinkedHashMap[String, Any]

val read  = implicitly[Read[Row]]  // will find an implicit Read instance
val write = implicitly[Write[Row]] // will find an implicit Write instance

case class Developer(
  id: Long, // serial
  fullName: String,
  age: Int
)

val builder = mutable.LinkedHashMap.newBuilder[String, Any]
builder += ("param_1" -> "tasty the tester")
builder += ("param_2" -> 42)
val parameters: Row = builder.result()

val insertString          = s"INSERT INTO developer (?, ?)"                        // statement with placeholders
val afr: ConnectionIO[Int] = write.toFragment(parameters, insertString).update.run // set placeholders using parameters

val selectString                  = s"SELECT full_name, age FROM developer" 
val data: ConnectionIO[List[Row]] = Fragment.const(selectString).query[Row](read).to[List]
```

In the insert example, we used the *toFragment* method of *Write*, but there are other ways to construct a sql statement
with given parameters; one approach will be discussed later when explaining *FragmentUtils*.
In the select example, we get back a *List[mutable.Map[String, Any]]*, and we can manipulate the map object as we like;
this feature can be used with Sangria
[Projector](https://github.com/sangria-graphql/sangria/blob/master/src/test/scala/sangria/execution/ProjectorSpec.scala)
to build dynamically loaded entities.

If you want to use select data using a fs2 *Stream* you can still use the *Read[Row]* provided here, but a more 
optimized version taken from Doobie examples is available:

```scala
Raw.processRaw(s"SELECT full_name, age FROM developer")
```

If you want to go back to case classes, you can use the *FromMap* typeclass. I discovered it on stack overflow
https://stackoverflow.com/questions/31640565/converting-mapstring-any-to-a-case-class and
https://stackoverflow.com/questions/55042252/shapeless-code-to-convert-mapstring-any-to-case-class-cannot-handle-optional.
It turns a *Map[String, Any]* into an *Option[T]*, where the *Option* is defined in the case of positive mapping.

```scala
import FromMap._

val valid: Map[String, Any]   = Map("id" -> 1L, "fullName" -> "tasty the tester", "age" -> 42)
val invalid: Map[String, Any] = Map("fullName" -> "tasty the tester", "age" -> 42)

ConvertHelper.to[Developer].from(valid)   // Some(x: Developer)
ConvertHelper.to[Developer].from(invalid) // None
```

## GenericRepository

When writing tests for your repositories you often need a way to setup or cleanup your database without using the
repository under test (otherwise the test makes little sense). This library provides a *GenericRepository* which is
a simple wrapper around Doobie query and update methods. The usage is quite simple, mainly because you have to provide
everything:

```scala
val repository = GenericRepository.forSchema("public")

repository.createSchema()

val tableDefinition = """|id         BIGSERIAL    PRIMARY KEY,
                         |label      VARCHAR(255) NOT NULL,
                         |created_at TIMESTAMP    NOT NULL,
                         |updated_at TIMESTAMP    NOT NULL,
                         |UNIQUE (label)""".stripMargin
repository.createTable("item", tableDefinition) 

repository.createSequence("item_seq", "START WITH 1 INCREMENT BY 1 NO CYCLE")

repository.select[Unit, (Long, String)](
  "item", 
  "id" :: "label" :: Nil, 
  "", 
  () // where parameters
)

repository.update[(String, Timestamp, Long)](
  "item", 
  "label" :: "updated_at" :: Nil, 
  "WHERE id = ?", 
  ("new label", Timestamp.from(Instant.now()), 1L) // set parameters + where parameters
)
``` 

Please do not use this for implementing real repositories. Its purpose is only to remove many of the test boilerplate, 
and the interface it offers is far from being user-friendly.

## TestTransactors

Since tests are important, this library offers a shorthand for creating a *Transactor* to use inside your tests. It
accepts a *DoobieConfig* which validates standard doobie configuration parameters. You must create an implementation 
for providing it to *TestTransactors*. If you do so, you can create a Doobie *Transactor* using:

```scala
TestTransactors.valueOf[IO](doobieConfig, TestTransactors.BlockerMode.Cached)
TestTransactors.valueOf[IO](doobieConfig, TestTransactors.BlockerMode.Fixed)
TestTransactors.valueOf[IO](doobieConfig, TestTransactors.BlockerMode.Sync)
```

The three are transactors backed by a cached thread pool, a fixed thread pool and no thread pool (sync) respectively.

## Others

I have included other utilities which don't need a deep explanation: *ExecutorServices*, *StringUtils*, *FragmentUtils*. 
The only one which is worth an example is the last one: it contains methods to interact with Doobie fragments. 
The first two methods are used to produce a *Query[W, R]* and an *Update[W]*, which are quite uncommon in Doobie 
because you usually work with *Query0[R]* and *Update0*, because it's a lot more common to set parameters on the fly 
using fr"" and sql"". If you decide to use these methods, you will need to provide a *W* in order to go back to the 
more common *Query0[R]* and *Update0* types:

```scala
def wrapInQuery[W: Write, R: Read](fragment: Fragment): Query[W, R]
def wrapInUpdate[W: Write](fragment: Fragment): Update[W]
```

The other three methods are used to combine fragments together in order to overcome the empty fragment problem. 
For example, if you use Doobie *Fragments.and(...)* methods with an empty *List*, you will get back the empty fragment; 
if you want to combine the previous result with another *Fragments.and(...)*, you will get a result like this: 
"() AND () AND () ..." because the empty fragment is seen as a valid fragment. 
To mitigate this problem, I decided to use the *Option* type as a guard:

```scala
def optionalAndOpt(fs: Option[Fragment]*): Option[Fragment]
def optionalOrOpt(fs: Option[Fragment]*): Option[Fragment]
```

If the list of *Option[Fragment]* is empty, a *None* is returned, otherwise a *Some(Fragments.and(...))* is returned. 
This behavior gives us the possibility to chain nested calls together and avoids producing invalid fragments. 
I have also added a method for creating an optional NOT fragment:

```scala
def optionalNotOpt(f: Option[Fragment]): Option[Fragment]
```
