package com.github.reddone.caseql.sql.table

import java.util.concurrent.atomic.AtomicLong

import com.github.reddone.caseql.sql.ItTestData._
import com.github.reddone.caseql.sql.ItTestModel._
import com.github.reddone.caseql.sql.ItTestImplicits._
import com.github.reddone.caseql.sql.PgAnyWordSpec
import com.github.reddone.caseql.sql.filter.models.IntFilter
import com.github.reddone.caseql.sql.util.TestTransactors._
import doobie.implicits._

class TableQueryItSpec extends PgAnyWordSpec {

  val currentDeveloperId = new AtomicLong(developers.length.toLong)
  val currentProjectId   = new AtomicLong(projects.length.toLong)
  val currentTaskId      = new AtomicLong(tasks.length.toLong)

  "TableQuery" when {

    "selecting data" should {

      "succeed to execute a simple select" in {
        val filter = DeveloperFilter.empty.copy(
          age = Some(IntFilter.empty.copy(EQ = Some(1)))
        )

        val developers = Table[Developer, DeveloperKey]
          .select(filter, Some("d"))
          .execute
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

        developers should contain theSameElementsAs List(
          Developer(2L, "Eddy Pasterino", 1, Some(1L)),
          Developer(3L, "Tasty the Tester", 1, Some(1L))
        )
      }

      "succeed to execute a select with a nested filter" in {
        val filter = DeveloperFilter.empty.copy(
          age = Some(IntFilter.empty.copy(EQ = Some(1)))
        )
      }

      "succeed to execute a select with a deep nested filter" in {}

      "succeed to execute a select with a relation filter" in {}

      "succeed to execute a select with a deep relation filter" in {}

      "succeed to execute a simple select by key" in {}
    }

    "inserting data" should {

      "succeed to execute a simple insert" in {}

      "succeed to execute an insert with null values" in {}

      "succeed to execute an insert with default values" in {}

      "succeed to execute an insert with missing values having a default" in {}

      "succeed to execute a simple insert returning key" in {}
    }

    "updating data" should {

      "succeed to execute a simple update" in {}

      "succeed to execute a simple update returning keys" in {}

      "succeed to execute a simple update by key" in {}

      "succeed to execute a simple update by key returning keys" in {}
    }

    "deleting data" should {

      "succeed to execute a simple delete" in {}

      "succeed to execute a simple delete returning keys" in {}

      "succeed to execute a simple delete by key" in {}

      "succeed to execute a simple delete by key returning keys" in {}
    }
  }
}
