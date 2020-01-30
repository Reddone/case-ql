package com.github.reddone.caseql.sql.hi

import cats.effect.IO
import doobie._
import doobie.implicits._
import Fragment._
import com.github.reddone.caseql.sql.PgAnyWordSpec
import com.github.reddone.caseql.sql.TestData._
import com.github.reddone.caseql.sql.repository.GenericRepository
import com.github.reddone.caseql.sql.util.Raw._

class RawStreamItSpec extends PgAnyWordSpec {

  val testRepository: GenericRepository = GenericRepository.forSchema(testSchema)

  "RawStream" when {

    "used to read raw data" should {

      "execute a Query with Stream" in {
        val result1: IO[List[Row]] = RawStream
          .processRaw(s"SELECT * FROM $testSchema.$developerTableName")
          .compile
          .toList
          .transact(xa)
        result1.unsafeRunSync().foreach(println)

        val result2: IO[List[Row]] = RawStream
          .processRaw(s"SELECT * FROM $testSchema.$projectTableName")
          .compile
          .toList
          .transact(xa)
        result2.unsafeRunSync().foreach(println)
      }
    }
  }
}
