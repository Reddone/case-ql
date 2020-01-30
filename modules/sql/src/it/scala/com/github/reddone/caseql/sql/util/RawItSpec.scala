package com.github.reddone.caseql.sql.util

import cats.effect.IO
import doobie.implicits._
import com.github.reddone.caseql.sql.PgAnyWordSpec
import com.github.reddone.caseql.sql.TestData._
import com.github.reddone.caseql.sql.repository.GenericRepository
import com.github.reddone.caseql.sql.util.Raw._

class RawItSpec extends PgAnyWordSpec {

  val testRepository: GenericRepository = GenericRepository.forSchema(testSchema)

  "Raw" when {

    "providing an implicit Read[Row]" should {

      "execute a Query with ConnectionIO" in {
        val result1: IO[List[Row]] = testRepository
          .select[Unit, Row](developerTableName, List("*"), "", ())
          .transact(xa)
        result1.unsafeRunSync().foreach(println)

        val result2: IO[List[Row]] = testRepository
          .select[Unit, Row](projectTableName, List("*"), "", ())
          .transact(xa)
        result2.unsafeRunSync().foreach(println)
      }

      "execute a Query with Stream" in {
        val result1: IO[List[Row]] = testRepository
          .selectStream[Unit, Row](developerTableName, List("*"), "", ())
          .compile
          .toList
          .transact(xa)
        result1.unsafeRunSync().foreach(println)

        val result2: IO[List[Row]] = testRepository
          .selectStream[Unit, Row](projectTableName, List("*"), "", ())
          .compile
          .toList
          .transact(xa)
        result2.unsafeRunSync().foreach(println)
      }
    }

    "providing an implicit Write[Row]" should {
      "b1" in {}
      "b2" in {}
    }
  }
}
