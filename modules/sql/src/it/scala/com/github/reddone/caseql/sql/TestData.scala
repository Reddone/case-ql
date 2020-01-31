package com.github.reddone.caseql.sql

import java.sql.Timestamp
import java.time.Instant

import com.github.reddone.caseql.sql.entity.models._
import shapeless._

object TestData {

  val testSchema: String = "test"

  // developer

  val developerTableName: String = "developer"

  val developerTableDefinition: String =
    """|id        BIGSERIAL PRIMARY KEY,
       |full_name VARCHAR(255) NOT NULL,
       |age       INTEGER NOT NULL
       |""".stripMargin

  val developerCols: List[String] = List("id", "full_name", "age")

  val developerColsNoId: List[String] = developerCols.tail

  val developers: List[Developer] = List(
    Developer(1L, "Eddy Pasterino", 999),
    Developer(2L, "Donger", 1),
    Developer(3L, "Reddone", 32)
  )

  val developersNoId: List[(String, Int)] = developers.map(Generic[Developer].to(_).tail.tupled)

  // project

  val projectTableName: String = "project"

  val projectTableDefinition: String =
    """|id          BIGSERIAL PRIMARY KEY,
       |title       VARCHAR(255) NOT NULL,
       |description VARCHAR(255) NULL,
       |created_at  TIMESTAMP NOT NULL,
       |updated_at  TIMESTAMP NOT NULL
       |""".stripMargin

  val projectCols: List[String] = List("id", "title", "description", "created_at", "updated_at")

  val projectColsNoId: List[String] = projectCols.tail

  val projects: List[Project] = List(
    Project(
      1L,
      "Topdeckin N' Wreckin",
      Some("Kripp most loved hobby"),
      Timestamp.from(Instant.EPOCH),
      Timestamp.from(Instant.EPOCH.plusSeconds(3600L))
    ),
    Project(
      2L,
      "Welcome to Summoner Rift",
      None,
      Timestamp.from(Instant.EPOCH),
      Timestamp.from(Instant.EPOCH.plusSeconds(7200L))
    )
  )

  val projectsNoId: List[(String, Option[String], Timestamp, Timestamp)] =
    projects.map(Generic[Project].to(_).tail.tupled)

  // developer_project_link

  val developerProjectLinkTableName: String = "developer_project_link"

  val developerProjectLinkTableDefinition: String =
    """|developer_id BIGINT NOT NULL REFERENCES test.developer (id) ON UPDATE CASCADE ON DELETE CASCADE,
       |project_id   BIGINT NOT NULL REFERENCES test.project (id) ON UPDATE CASCADE ON DELETE CASCADE
       |""".stripMargin

  val developerProjectLinkCols: List[String] = List("developer_id", "project_id")

  val developerProjectLinks: List[DeveloperProjectLink] = List(
    DeveloperProjectLink(1L, 1L),
    DeveloperProjectLink(2L, 2L),
    DeveloperProjectLink(3L, 1L),
    DeveloperProjectLink(3L, 2L)
  )
}
