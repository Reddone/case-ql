package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.ItTestModel._
import com.github.reddone.caseql.sql.ItTestImplicits._
import com.github.reddone.caseql.sql.PgAnyWordSpec
import com.github.reddone.caseql.sql.util.TestTransactors._
import doobie.implicits._

class TableQueryItSpec extends PgAnyWordSpec {

  "TableQuery" when {

    "selecting data" should {

      "succeed to execute a simple select" in {
        val developers = Table[Developer, DeveloperKey]
          .select(null, Some("dev"))
          .execute
          .transact(rollingBack(xa))
          .compile
          .toList
          .unsafeRunSync()

      }

      "succeed to execute a select with a nested filter" in {}

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
