package com.github.reddone.caseql.sql.util

import java.util.concurrent.atomic.AtomicLong

import cats.implicits._
import doobie._
import doobie.implicits._
import com.github.reddone.caseql.sql.PgAnyWordSpec
import com.github.reddone.caseql.sql.ItTestData._
import com.github.reddone.caseql.sql.util.Raw._
import com.github.reddone.caseql.sql.util.TestTransactors._

import scala.collection.mutable

class RawItSpec extends PgAnyWordSpec {

  val testRepository: GenericRepository = GenericRepository.forSchema(testSchema)

  val nextDeveloperId = new AtomicLong(developers.length + 1L)
  val nextProjectId   = new AtomicLong(projects.length + 1L)

  val rawDevelopers: List[Map[String, Any]] = List(
    Map("id" -> 1L, "name" -> "Reddone", "age"                   -> 32, "team_leader_id" -> None),
    Map("id" -> 2L, "name" -> "Eddy Pasterino", "age"            -> 1, "team_leader_id"  -> Some(1L)),
    Map("id" -> 3L, "name" -> "Tasty the Tester", "age"          -> 1, "team_leader_id"  -> Some(1L)),
    Map("id" -> 4L, "name" -> "Maximus Kappacus Spamicus", "age" -> 23, "team_leader_id" -> None),
    Map("id" -> 5L, "name" -> "Cyberino Matterino", "age"        -> 2, "team_leader_id"  -> Some(4L)),
    Map("id" -> 6L, "name" -> "Mario Rigatone", "age"            -> 2, "team_leader_id"  -> Some(4L))
  )

  val rawProjects: List[Map[String, Any]] = List(
    Map("id" -> 1L, "title" -> "Topdeckin N' Wreckin", "description"     -> Some("Kripp most loved hobby")),
    Map("id" -> 2L, "title" -> "Random Pasta", "description"             -> None),
    Map("id" -> 3L, "title" -> "Welcome to Summoner Rift", "description" -> Some("Fedding like there's no tomorrow"))
  )

  "Raw" when {

    "producing a stream of Row" should {

      "succeed to execute a query returning Stream" in {
        val result1 = Raw
          .processRaw(s"SELECT * FROM $testSchema.$developerTableName")
          .compile
          .toList
          .transact(rollingBack(xa))
          .unsafeRunSync()

        result1 shouldBe rawDevelopers

        val result2 = Raw
          .processRaw(s"SELECT * FROM $testSchema.$projectTableName")
          .compile
          .toList
          .transact(rollingBack(xa))
          .unsafeRunSync()

        result2 shouldBe rawProjects
      }
    }

    "providing an implicit Read[Row]" should {

      "succeed to execute a query returning ConnectionIO" in {
        val result1 = testRepository
          .select[Unit, Row](developerTableName, List("*"), "", ())
          .transact(rollingBack(xa))
          .unsafeRunSync()

        result1 shouldBe rawDevelopers

        val result2 = testRepository
          .select[Unit, Row](projectTableName, List("*"), "", ())
          .transact(rollingBack(xa))
          .unsafeRunSync()

        result2 shouldBe rawProjects
      }

      "succeed to execute a query returning Stream" in {
        val result1 = testRepository
          .selectStream[Unit, Row](developerTableName, List("*"), "", ())
          .compile
          .toList
          .transact(rollingBack(xa))
          .unsafeRunSync()

        result1 shouldBe rawDevelopers

        val result2 = testRepository
          .selectStream[Unit, Row](projectTableName, List("*"), "", ())
          .compile
          .toList
          .transact(rollingBack(xa))
          .unsafeRunSync()

        result2 shouldBe rawProjects
      }
    }

    "providing an implicit Write[Row]" should {

      "succeed to execute an update when parameters are correct" in {
        val builder1 = mutable.LinkedHashMap.newBuilder[String, Any]
        builder1 += ("param_1" -> "A random Donger appears")
        builder1 += ("param_2" -> 42)
        builder1 += ("param_3" -> None)
        val parameters1: Row = builder1.result()

        val (nextId, result1) = (for {
          _   <- testRepository.insert(developerTableName, developerColsNoId, parameters1)
          id  <- nextDeveloperId.getAndIncrement().pure[ConnectionIO]
          dev <- testRepository.select[Unit, Row](developerTableName, List("*"), s"WHERE id = $id", ())
        } yield (id, dev)).transact(rollingBack(xa)).unsafeRunSync()

        result1 shouldBe List(
          Map("id" -> nextId, "name" -> "A random Donger appears", "age" -> 42, "team_leader_id" -> None)
        )
      }

      "fail to execute an update when parameters are wrong" in {
        val builder1 = mutable.LinkedHashMap.newBuilder[String, Any]
        builder1 += ("param_1" -> "A random Donger appears")
        builder1 += ("param_2" -> 42)
        builder1 += ("param_3" -> None)
        builder1 += ("param_4" -> "Kaboom")
        val parameters1: Row = builder1.result()

        val thrown1 = the[IllegalArgumentException] thrownBy testRepository
          .insert(developerTableName, developerColsNoId, parameters1)
          .transact(rollingBack(xa))
          .unsafeRunSync()

        thrown1.getMessage shouldBe "You are trying to set 3 parameters using 4 values"

        val builder2 = mutable.LinkedHashMap.newBuilder[String, Any]
        builder2 += ("param_1" -> "A random Donger appears")
        builder2 += ("param_2" -> "Kaboom")
        val parameters2: Row = builder2.result()

        val thrown2 = the[IllegalArgumentException] thrownBy testRepository
          .insert(developerTableName, developerColsNoId, parameters2)
          .transact(rollingBack(xa))
          .unsafeRunSync()

        thrown2.getMessage shouldBe "You are trying to set 3 parameters using 2 values"
      }
    }
  }
}
