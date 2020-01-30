package com.github.reddone.caseql.sql

import java.sql.Timestamp
import java.time.Instant

import com.github.reddone.caseql.sql.entity.models._

object TestData {

  val developerTableDefinition: String =
    """|
       |
       |""".stripMargin

  val developers: List[Developer] = List(
    Developer(1L, "Eddy Pasterino", 999),
    Developer(2L, "Donger", 1),
    Developer(3L, "Reddone", 32)
  )

  val projectTableDefinition: String =
    """|
       |
       |""".stripMargin

  val projects: List[Project] = List(
    Project(
      1L,
      "Topdeckin N' Wreckin",
      "Kripp most loved hobby",
      Timestamp.from(Instant.EPOCH),
      Timestamp.from(Instant.EPOCH.plusSeconds(3600L))
    ),
    Project(
      2L,
      "Welcome to Summoner Rift",
      "Make Reddone not tilt again",
      Timestamp.from(Instant.EPOCH),
      Timestamp.from(Instant.EPOCH.plusSeconds(7200L))
    )
  )

  val projectTablesDefinition: String =
    """|
       |
       |""".stripMargin

  val developerProjects: List[DeveloperProjectLink] = List(
    DeveloperProjectLink(1L, 1L),
    DeveloperProjectLink(2L, 2L),
    DeveloperProjectLink(3L, 1L),
    DeveloperProjectLink(3L, 2L)
  )
}
