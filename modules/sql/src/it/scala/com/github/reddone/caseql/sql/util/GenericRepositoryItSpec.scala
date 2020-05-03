package com.github.reddone.caseql.sql.util

import java.util.concurrent.atomic.AtomicLong

import com.github.reddone.caseql.sql.itmodel.data._
import com.github.reddone.caseql.sql.itmodel.db._
import com.github.reddone.caseql.sql.{PgAnyWordSpec, PgHelper}
import com.github.reddone.caseql.sql.util.TestTransactors._
import doobie.implicits._

class GenericRepositoryItSpec extends PgAnyWordSpec {

  val anotherSchema   = "another_schema"
  val anotherTable    = "another_table"
  val anotherSequence = "another_sequence"

  val testRepository: GenericRepository        = GenericRepository.forSchema(testSchema)
  val anotherTestRepository: GenericRepository = GenericRepository.forSchema(anotherSchema)

  val currentDeveloperId = new AtomicLong(developers.length.toLong)
  val currentProjectId   = new AtomicLong(projects.length.toLong)
  val currentTaskId      = new AtomicLong(tasks.length.toLong)

  "GenericRepository" when {

    "using schema DDL" should {

      "succeed to create a schema" in {
        val createSchema = anotherTestRepository.createSchema()
        val checkSchema  = PgHelper.checkSchema(anotherSchema).option

        val action = for {
          _                 <- createSchema
          schemaAfterCreate <- checkSchema
        } yield schemaAfterCreate

        val maybeSchemaAfterCreate = action
          .transact(rollingBack(xa))
          .unsafeRunSync()

        maybeSchemaAfterCreate shouldBe defined
        maybeSchemaAfterCreate.get shouldBe anotherSchema
      }

      "succeed to delete a schema" in {
        val createSchema = PgHelper.createSchema(anotherSchema).run
        val deleteSchema = anotherTestRepository.dropSchema()
        val checkSchema  = PgHelper.checkSchema(anotherSchema).option

        val action = for {
          _                 <- createSchema
          schemaAfterCreate <- checkSchema
          _                 <- deleteSchema
          schemaAfterDelete <- checkSchema
        } yield (schemaAfterCreate, schemaAfterDelete)

        val (maybeSchemaAfterCreate, maybeSchemaAfterDelete) = action
          .transact(rollingBack(xa))
          .unsafeRunSync()

        maybeSchemaAfterCreate shouldBe defined
        maybeSchemaAfterCreate.get shouldBe anotherSchema
        maybeSchemaAfterDelete shouldBe empty
      }
    }

    "using table DDL" should {

      "succeed to create a table" in {
        val createTable = testRepository.createTable(anotherTable, "id BIGINT PRIMARY KEY")
        val checkTable  = PgHelper.checkTable(testSchema, anotherTable).option

        val action = for {
          _                <- createTable
          tableAfterCreate <- checkTable
        } yield tableAfterCreate

        val maybeTableAfterCreate = action
          .transact(rollingBack(xa))
          .unsafeRunSync()

        maybeTableAfterCreate shouldBe defined
        maybeTableAfterCreate.get shouldBe defined
        maybeTableAfterCreate.get.get shouldBe anotherTable
      }

      "succeed to delete a table" in {
        val createTable = PgHelper.createTable(testSchema, anotherTable).run
        val deleteTable = testRepository.dropTable(anotherTable)
        val checkTable  = PgHelper.checkTable(testSchema, anotherTable).option

        val action = for {
          _                <- createTable
          tableAfterCreate <- checkTable
          _                <- deleteTable
          tableAfterDelete <- checkTable
        } yield (tableAfterCreate, tableAfterDelete)

        val (maybeTableAfterCreate, maybeTableAfterDelete) = action
          .transact(rollingBack(xa))
          .unsafeRunSync()

        maybeTableAfterCreate shouldBe defined
        maybeTableAfterCreate.get shouldBe defined
        maybeTableAfterCreate.get.get shouldBe anotherTable
        maybeTableAfterDelete shouldBe defined
        maybeTableAfterDelete.get shouldBe empty
      }
    }

    "using sequence DDL" should {

      "succeed to create a sequence" in {
        val createSequence = testRepository.createSequence(anotherSequence, "START WITH 1 INCREMENT BY 1 NO CYCLE")
        val checkSequence  = PgHelper.checkSequence(testSchema, anotherSequence).option

        val action = for {
          _                   <- createSequence
          sequenceAfterCreate <- checkSequence
        } yield sequenceAfterCreate

        val maybeSequenceAfterCreate = action
          .transact(rollingBack(xa))
          .unsafeRunSync()

        maybeSequenceAfterCreate shouldBe defined
        maybeSequenceAfterCreate.get shouldBe defined
        maybeSequenceAfterCreate.get.get shouldBe anotherSequence
      }

      "succeed to delete a sequence" in {
        val createSequence = PgHelper.createSequence(testSchema, anotherSequence).run
        val deleteSequence = testRepository.dropSequence(anotherSequence)
        val checkSequence  = PgHelper.checkSequence(testSchema, anotherSequence).option

        val action = for {
          _                   <- createSequence
          sequenceAfterCreate <- checkSequence
          _                   <- deleteSequence
          sequenceAfterDelete <- checkSequence
        } yield (sequenceAfterCreate, sequenceAfterDelete)

        val (maybeSequenceAfterCreate, maybeSequenceAfterDelete) = action
          .transact(rollingBack(xa))
          .unsafeRunSync()

        maybeSequenceAfterCreate shouldBe defined
        maybeSequenceAfterCreate.get shouldBe defined
        maybeSequenceAfterCreate.get.get shouldBe anotherSequence
        maybeSequenceAfterDelete shouldBe defined
        maybeSequenceAfterDelete.get shouldBe empty
      }
    }

    "using select DML" should {

      "succeed to execute a query returning ConnectionIO" in {
        val selectDevelopers = testRepository.select[(String, Int), Developer](
          developerTableName,
          "*" :: Nil,
          "where name=? or age=?",
          ("Reddone", 1)
        )

        val developers = selectDevelopers
          .transact(rollingBack(xa))
          .unsafeRunSync()

        developers.length shouldBe 3
        developers should contain theSameElementsAs List(
          Developer(1L, "Reddone", 32, None),
          Developer(2L, "Eddy Pasterino", 1, Some(1L)),
          Developer(3L, "Tasty the Tester", 1, Some(1L))
        )
      }

      "succeed to execute an update returning Stream" in {
        val selectDevelopers = testRepository.selectStream[(String, Int), Developer](
          developerTableName,
          "*" :: Nil,
          "where name=? or age=?",
          ("Reddone", 1)
        )

        val developers = selectDevelopers
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

        developers.length shouldBe 3
        developers should contain theSameElementsAs List(
          Developer(1L, "Reddone", 32, None),
          Developer(2L, "Eddy Pasterino", 1, Some(1L)),
          Developer(3L, "Tasty the Tester", 1, Some(1L))
        )
      }
    }

    "using insert DML" should {

      "succeed to execute an insert returning ConnectionIO of affected row" in {
        val nextDeveloperId = currentDeveloperId.incrementAndGet()

        val insertDeveloper = testRepository.insert[Developer](
          developerTableName,
          developerCols,
          Developer(nextDeveloperId, "Young Donger", 1, None)
        )

        val affectedRows = insertDeveloper
          .transact(rollingBack(xa))
          .unsafeRunSync()

        affectedRows shouldBe 1
      }

      "succeed to execute an insert returning ConnectionIO of unique key" in {
        val nextDeveloperId = currentDeveloperId.incrementAndGet()

        val insertDeveloperReturningKey = testRepository.insertReturningKey[Developer, Long](
          developerTableName,
          developerCols,
          Developer(nextDeveloperId, "Young Donger", 1, None),
          developerCols.head :: Nil
        )

        val returnedKey = insertDeveloperReturningKey
          .transact(rollingBack(xa))
          .unsafeRunSync()

        returnedKey shouldBe nextDeveloperId
      }

      "succeed to execute a batch insert returning ConnectionIO of affected rows" in {
        val nextDeveloperId1 = currentDeveloperId.incrementAndGet()
        val nextDeveloperId2 = currentDeveloperId.incrementAndGet()

        val insertDevelopers = testRepository.insertMany[Developer](
          developerTableName,
          developerCols,
          Developer(nextDeveloperId1, "Young Donger", 1, None) ::
            Developer(nextDeveloperId2, "Old Donger", 2, Some(4L)) :: Nil
        )

        val affectedRows = insertDevelopers
          .transact(rollingBack(xa))
          .unsafeRunSync()

        affectedRows shouldBe 2
      }

      "succeed to execute a batch insert returning Stream of keys" in {
        val nextDeveloperId1 = currentDeveloperId.incrementAndGet()
        val nextDeveloperId2 = currentDeveloperId.incrementAndGet()

        val insertDevelopersReturningKeys = testRepository.insertManyReturningKeys[Developer, Long](
          developerTableName,
          developerCols,
          Developer(nextDeveloperId1, "Young Donger", 1, None) ::
            Developer(nextDeveloperId2, "Old Donger", 2, Some(4L)) :: Nil,
          developerCols.head :: Nil
        )

        val returnedKeys = insertDevelopersReturningKeys
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

        returnedKeys should contain theSameElementsAs List(nextDeveloperId1, nextDeveloperId2)
      }
    }

    "using update DML" should {

      "succeed to execute an update returning ConnectionIO of affected rows" in {
        val updateDeveloper = testRepository.update[(String, Int, Option[Long], Long)](
          developerTableName,
          developerColsNoId,
          "where id=?",
          ("Minimus Lambdacus Spamicus", 1, Some(1L), 4L)
        )

        val affectedRows = updateDeveloper
          .transact(rollingBack(xa))
          .unsafeRunSync()

        affectedRows shouldBe 1
      }

      "succeed to execute an update returning Stream of keys" in {
        val updateDeveloperReturningKeys =
          testRepository.updateReturningKeys[(String, Int, Option[Long], Long), Long](
            developerTableName,
            developerColsNoId,
            "where id=?",
            ("Minimus Lambdacus Spamicus", 1, Some(1L), 4L),
            developerCols.head :: Nil
          )

        val returnedKeys = updateDeveloperReturningKeys
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

        returnedKeys should contain theSameElementsAs List(4L)
      }

      "succeed to execute a batch update returning ConnectionIO of affected rows" in {
        val updateDevelopers = testRepository.updateMany[(String, Int, Option[Long], Long)](
          developerTableName,
          developerColsNoId,
          "where id=?",
          ("Minimus Lambdacus Spamicus", 1, Some(1L), 4L) ::
            ("Luigi Spaghetti", 1, None, 6L) :: Nil
        )

        val affectedRows = updateDevelopers
          .transact(rollingBack(xa))
          .unsafeRunSync()

        affectedRows shouldBe 2
      }

      "succeed to execute a batch update returning Stream of keys" in {
        val updateDevelopersReturningKeys =
          testRepository.updateManyReturningKeys[(String, Int, Option[Long], Long), Long](
            developerTableName,
            developerColsNoId,
            "where id=?",
            ("Minimus Lambdacus Spamicus", 1, Some(1L), 4L) ::
              ("Luigi Spaghetti", 1, None, 6L) :: Nil,
            developerCols.head :: Nil
          )

        val returnedKeys = updateDevelopersReturningKeys
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

        returnedKeys should contain theSameElementsAs List(4L, 6L)
      }
    }

    "using delete DML" should {

      "succeed to execute a delete returning ConnectionIO of affected rows" in {
        val deleteDeveloper = testRepository.delete[Long](
          developerTableName,
          "where id=?",
          4L
        )

        val affectedRows = deleteDeveloper
          .transact(rollingBack(xa))
          .unsafeRunSync()

        affectedRows shouldBe 1
      }

      "succeed to execute a delete returning Stream of keys" in {
        val deleteDeveloperReturningKeys = testRepository.deleteReturningKeys[Long, Long](
          developerTableName,
          "where id=?",
          4L,
          developerCols.head :: Nil
        )

        val returnedKeys = deleteDeveloperReturningKeys
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

        returnedKeys should contain theSameElementsAs List(4L)
      }

      "succeed to execute a batch delete returning ConnectionIO of affected rows" in {
        val deleteDevelopers = testRepository.deleteMany[Int](
          developerTableName,
          "where age=?",
          2 :: Nil
        )

        val affectedRows = deleteDevelopers
          .transact(rollingBack(xa))
          .unsafeRunSync()

        affectedRows shouldBe 2
      }

      "succeed to execute a batch delete returning Stream of keys" in {
        val deleteDevelopersReturningKeys = testRepository.deleteManyReturningKeys[Int, Long](
          developerTableName,
          "where age=?",
          2 :: Nil,
          developerCols.head :: Nil
        )

        val returnedKeys = deleteDevelopersReturningKeys
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

        returnedKeys should contain theSameElementsAs List(5L, 6L)
      }
    }
  }
}
