package com.github.reddone.caseql.sql.repository

import java.util.concurrent.atomic.AtomicLong

import com.github.reddone.caseql.sql.{PgAnyWordSpec, PgHelper}
import com.github.reddone.caseql.sql.ItTestData.{developers, projects, testSchema}
import doobie._
import doobie.implicits._

class GenericRepositoryItSpec extends PgAnyWordSpec {

  val anotherTestSchema = "another"

  val testRepository: GenericRepository        = GenericRepository.forSchema(testSchema)
  val anotherTestRepository: GenericRepository = GenericRepository.forSchema(anotherTestSchema)

  val nextDeveloperId = new AtomicLong(developers.length + 1L)
  val nextProjectId   = new AtomicLong(projects.length + 1L)

  "GenericRepository" when {

    "using schema DDL" should {

      "succeed to create a schema" in {
        anotherTestRepository
          .createSchema()
          .transact(xa)
          .unsafeRunSync()

        PgHelper
          .checkSchema(anotherTestSchema)
          .option
          .transact(xa)
          .unsafeRunSync() shouldBe defined

        PgHelper
          .dropSchema(anotherTestSchema)
          .run
          .transact(xa)
          .unsafeRunSync()
      }

      "succeed to delete a schema" in {
        PgHelper
          .createSchema(anotherTestSchema)
          .run
          .transact(xa)
          .unsafeRunSync()

        anotherTestRepository
          .dropSchema()
          .transact(xa)
          .unsafeRunSync()

        PgHelper
          .checkSchema(anotherTestSchema)
          .option
          .transact(xa)
          .unsafeRunSync() shouldBe empty
      }
    }

    "using table DDL" should {

      "succeed to create a table" in {
        testRepository
          .createTable("anotherTable", "id BIGINT PRIMARY KEY")
          .transact(xa)
          .unsafeRunSync()

        PgHelper
          .checkTable(testSchema, "anotherTable")
          .option
          .transact(xa)
          .unsafeRunSync()
          .flatten shouldBe defined

        PgHelper
          .dropTable(testSchema, "anotherTable")
          .run
          .transact(xa)
          .unsafeRunSync()
      }

      "succeed to delete a table" in {
        PgHelper
          .createTable(testSchema, "anotherTable")
          .run
          .transact(xa)
          .unsafeRunSync()

        testRepository
          .dropTable("anotherTable")
          .transact(xa)
          .unsafeRunSync()

        PgHelper
          .checkTable(testSchema, "anotherTable")
          .option
          .transact(xa)
          .unsafeRunSync()
          .flatten shouldBe empty
      }
    }

    "using sequence DDL" should {

      "succeed to create a sequence" in {
        testRepository
          .createSequence("anotherSequence", "START WITH 1 INCREMENT BY 1 NO CYCLE")
          .transact(xa)
          .unsafeRunSync()

        PgHelper
          .checkSequence(testSchema, "anotherSequence")
          .option
          .transact(xa)
          .unsafeRunSync()
          .flatten shouldBe defined

        PgHelper
          .dropSequence(testSchema, "anotherSequence")
          .run
          .transact(xa)
          .unsafeRunSync()
      }

      "succeed to delete a sequence" in {
        PgHelper
          .createSequence(testSchema, "anotherSequence")
          .run
          .transact(xa)
          .unsafeRunSync()

        testRepository
          .dropSequence("anotherSequence")
          .transact(xa)
          .unsafeRunSync()

        PgHelper
          .checkSequence(testSchema, "anotherSequence")
          .option
          .transact(xa)
          .unsafeRunSync()
          .flatten shouldBe empty
      }
    }

    "using select DML" should {

      "succeed to execute a query returning ConnectionIO" in {}

      "succeed to execute an update returning Stream" in {}
    }

    "using insert DML" should {

      "succeed to execute an update returning ConnectionIO of affected rows" in {}

      "succeed to execute an update returning Stream of unique keys" in {}

      "succeed to execute an update returning Stream of keys" in {}

      "succeed to execute a batch update returning ConnectionIO of affected rows" in {}

      "succeed to execute a batch update returning Stream of keys" in {}
    }

    "using update DML" should {

      "succeed to execute an update returning ConnectionIO of affected rows" in {}

      "succeed to execute an update returning Stream of unique keys" in {}

      "succeed to execute an update returning Stream of keys" in {}

      "succeed to execute a batch update returning ConnectionIO of affected rows" in {}

      "succeed to execute a batch update returning Stream of keys" in {}
    }

    "using delete DML" should {

      "succeed to execute an update returning ConnectionIO of affected rows" in {}

      "succeed to execute an update returning Stream of unique keys" in {}

      "succeed to execute an update returning Stream of keys" in {}

      "succeed to execute a batch update returning ConnectionIO of affected rows" in {}

      "succeed to execute a batch update returning Stream of keys" in {}
    }
  }
}
