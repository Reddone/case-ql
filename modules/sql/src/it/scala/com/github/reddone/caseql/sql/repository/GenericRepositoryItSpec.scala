package com.github.reddone.caseql.sql.repository

import java.util.concurrent.atomic.AtomicLong

import com.github.reddone.caseql.sql.PgAnyWordSpec
import com.github.reddone.caseql.sql.TestData.{developers, projects, testSchema}

class GenericRepositoryItSpec extends PgAnyWordSpec {

  val testRepository: GenericRepository        = GenericRepository.forSchema(testSchema)
  val anotherTestRepository: GenericRepository = GenericRepository.forSchema("another")

  val nextDeveloperId = new AtomicLong(developers.length + 1L)
  val nextProjectId   = new AtomicLong(projects.length + 1L)

  "GenericRepository" when {

    "using schema DDL" should {

      "succeed to create a schema" in {}

      "succeed to delete a schema" in {}
    }

    "using table DDL" should {

      "succeed to create a table" in {}

      "succeed to delete a table" in {}
    }

    "using sequence DDL" should {

      "succeed to create a sequence" in {}

      "succeed to delete a sequence" in {}
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
