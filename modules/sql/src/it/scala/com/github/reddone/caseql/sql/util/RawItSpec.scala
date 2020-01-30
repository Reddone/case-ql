package com.github.reddone.caseql.sql.util

import cats.effect.IO
import doobie.implicits._
import com.github.reddone.caseql.sql.PgAnyWordSpec
import com.github.reddone.caseql.sql.TestData._
import com.github.reddone.caseql.sql.repository.GenericRepository
import com.github.reddone.caseql.sql.util.Raw._

import scala.collection.mutable

class RawItSpec extends PgAnyWordSpec {

  val testRepository: GenericRepository = GenericRepository.forSchema(testSchema)

  "Raw" when {

    "providing an implicit Read[Row]" should {

      "succeed to execute a query returning ConnectionIO" in {
        val result1: IO[List[Row]] = testRepository
          .select[Unit, Row](developerTableName, List("*"), "", ())
          .transact(xa)
        result1.unsafeRunSync().foreach(println)

        val result2: IO[List[Row]] = testRepository
          .select[Unit, Row](projectTableName, List("*"), "", ())
          .transact(xa)
        result2.unsafeRunSync().foreach(println)
      }

      "succeed to execute a query returning Stream" in {
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

      "succeed to execute an update when parameters are correct" in {
        val builder =  mutable.LinkedHashMap.newBuilder[String, Any]
        builder += ("full_name" -> "tasty the tester")
        builder += ("age" -> 42)
        val parameters: Row = builder.result()

        val result1 = testRepository
          .insert(developerTableName, developerColsNoId, parameters)
          .transact(xa)
          .unsafeRunSync()

        println(result1)
      }

      "fail to execute an update when parameters are wrong" in {
        val parameters = null
      }
    }
  }
}
