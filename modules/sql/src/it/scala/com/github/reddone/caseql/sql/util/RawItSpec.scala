package com.github.reddone.caseql.sql.util

import java.sql.Timestamp
import java.time.Instant
import java.util.concurrent.atomic.AtomicLong

import cats.implicits._
import doobie._
import doobie.implicits._
import com.github.reddone.caseql.sql.PgAnyWordSpec
import com.github.reddone.caseql.sql.TestData._
import com.github.reddone.caseql.sql.repository.GenericRepository
import com.github.reddone.caseql.sql.util.Raw._

import scala.collection.mutable

class RawItSpec extends PgAnyWordSpec {

  val testRepository: GenericRepository = GenericRepository.forSchema(testSchema)

  val nextDeveloperId = new AtomicLong(developers.length + 1L)
  val nextProjectId   = new AtomicLong(projects.length + 1L)

  val rawDevelopers: List[Map[String, Any]] = List(
    Map("id" -> 1, "full_name" -> "Eddy Pasterino", "age" -> 999),
    Map("id" -> 2, "full_name" -> "Donger", "age"         -> 1),
    Map("id" -> 3, "full_name" -> "Reddone", "age"        -> 32)
  )

  val rawProjects: List[Map[String, Any]] = List(
    Map(
      "id"          -> 1,
      "title"       -> "Topdeckin N' Wreckin",
      "description" -> Some("Kripp most loved hobby"),
      "created_at"  -> Timestamp.from(Instant.EPOCH),
      "updated_at"  -> Timestamp.from(Instant.EPOCH.plusSeconds(3600L))
    ),
    Map(
      "id"          -> 2,
      "title"       -> "Welcome to Summoner Rift",
      "description" -> None,
      "created_at"  -> Timestamp.from(Instant.EPOCH),
      "updated_at"  -> Timestamp.from(Instant.EPOCH.plusSeconds(7200L))
    )
  )

  "Raw" when {

    "producing a stream of Row" should {

      "succeed to execute a query returning Stream" in {
        val result1 = Raw
          .processRaw(s"SELECT * FROM $testSchema.$developerTableName")
          .compile
          .toList
          .transact(xa)
          .unsafeRunSync()

        result1 shouldBe rawDevelopers

        val result2 = Raw
          .processRaw(s"SELECT * FROM $testSchema.$projectTableName")
          .compile
          .toList
          .transact(xa)
          .unsafeRunSync()

        result2 shouldBe rawProjects
      }
    }

    "providing an implicit Read[Row]" should {

      "succeed to execute a query returning ConnectionIO" in {
        val result1 = testRepository
          .select[Unit, Row](developerTableName, List("*"), "", ())
          .transact(xa)
          .unsafeRunSync()

        result1 shouldBe rawDevelopers

        val result2 = testRepository
          .select[Unit, Row](projectTableName, List("*"), "", ())
          .transact(xa)
          .unsafeRunSync()

        result2 shouldBe rawProjects
      }

      "succeed to execute a query returning Stream" in {
        val result1 = testRepository
          .selectStream[Unit, Row](developerTableName, List("*"), "", ())
          .compile
          .toList
          .transact(xa)
          .unsafeRunSync()

        result1 shouldBe rawDevelopers

        val result2 = testRepository
          .selectStream[Unit, Row](projectTableName, List("*"), "", ())
          .compile
          .toList
          .transact(xa)
          .unsafeRunSync()

        result2 shouldBe rawProjects
      }
    }

    "providing an implicit Write[Row]" should {

      "succeed to execute an update when parameters are correct" in {
        val builder1 = mutable.LinkedHashMap.newBuilder[String, Any]
        builder1 += ("param_1" -> "tasty the tester")
        builder1 += ("param_2" -> 42)
        val parameters1: Row = builder1.result()

        val nextId = (for {
          _  <- testRepository.insert(developerTableName, developerColsNoId, parameters1)
          id <- nextDeveloperId.getAndIncrement().pure[ConnectionIO]
        } yield id).transact(xa).unsafeRunSync()
        val result1 = (for {
          dev <- testRepository.select[Unit, Row](developerTableName, List("*"), s"WHERE id = $nextId", ())
          _   <- testRepository.delete(developerTableName, s"WHERE id = $nextId", ())
        } yield dev).transact(xa).unsafeRunSync()

        result1 shouldBe List(Map("id" -> nextId, "full_name" -> "tasty the tester", "age" -> 42))
      }

      "fail to execute an update when parameters are wrong" in {
        val builder1 = mutable.LinkedHashMap.newBuilder[String, Any]
        builder1 += ("param_1" -> "tasty the tester")
        builder1 += ("param_2" -> 42)
        builder1 += ("param_3" -> "Kaboom")
        val parameters1: Row = builder1.result()

        val thrown1 = the[IllegalArgumentException] thrownBy testRepository
          .insert(developerTableName, developerColsNoId, parameters1)
          .transact(xa)
          .unsafeRunSync()

        thrown1.getMessage shouldBe "You are trying to set 2 parameters using 3 values"

        val builder2 = mutable.LinkedHashMap.newBuilder[String, Any]
        builder2 += ("param_1" -> "Kaboom")
        val parameters2: Row = builder2.result()

        val thrown2 = the[IllegalArgumentException] thrownBy testRepository
          .insert(developerTableName, developerColsNoId, parameters2)
          .transact(xa)
          .unsafeRunSync()

        thrown2.getMessage shouldBe "You are trying to set 2 parameters using 1 values"
      }
    }
  }
}
