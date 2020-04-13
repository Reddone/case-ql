package com.github.reddone.caseql.sql

import java.sql.Timestamp
import java.time.Instant

import com.github.reddone.caseql.sql.ItTestModel._

object ItTestData {

  val testSchema: String = "test"

  // developer

  val developerTableName: String = "developer"

  val developerTableDefinition: String =
    """|id             BIGSERIAL PRIMARY KEY,
       |name           VARCHAR(255) NOT NULL,
       |age            INTEGER NOT NULL,
       |team_leader_id BIGINT NULL REFERENCES test.developer (id) ON UPDATE CASCADE ON DELETE SET NULL
       |""".stripMargin

  val developerCols: List[String] = List("id", "name", "age", "team_leader_id")

  val developerColsNoId: List[String] = developerCols.tail

  val developers: List[Developer] = List(
    Developer(1L, "Reddone", 32, None),
    Developer(2L, "Eddy Pasterino", 1, Some(1L)),
    Developer(3L, "Tasty the Tester", 1, Some(1L)),
    Developer(4L, "Maximus Kappacus Spamicus", 23, None),
    Developer(5L, "Cyberino Matterino", 2, Some(4L)),
    Developer(6L, "Mario Rigatone", 2, Some(4L))
  )

  val developersNoId: List[(String, Int, Option[Long])] =
    developers.map(d => (d.name, d.age, d.teamLeaderId))

  // project

  val projectTableName: String = "project"

  val projectTableDefinition: String =
    """|id          BIGSERIAL PRIMARY KEY,
       |title       VARCHAR(255) NOT NULL,
       |description TEXT NULL
       |""".stripMargin

  val projectCols: List[String] = List("id", "title", "description")

  val projectColsNoId: List[String] = projectCols.tail

  val projects: List[Project] = List(
    Project(
      1L,
      "Topdeckin N' Wreckin",
      Some("Kripp most loved hobby")
    ),
    Project(
      2L,
      "Random Pasta",
      None
    ),
    Project(
      3L,
      "Welcome to Summoner Rift",
      Some("Fedding like there's no tomorrow")
    )
  )

  val projectsNoId: List[(String, Option[String])] =
    projects.map(p => (p.title, p.description))

  // developer_project_link

  val developerProjectLinkTableName: String = "developer_project_link"

  val developerProjectLinkTableDefinition: String =
    """|developer_id BIGINT NOT NULL REFERENCES test.developer (id) ON UPDATE CASCADE ON DELETE CASCADE,
       |project_id   BIGINT NOT NULL REFERENCES test.project (id) ON UPDATE CASCADE ON DELETE CASCADE
       |""".stripMargin

  val developerProjectLinkCols: List[String] = List("developer_id", "project_id")

  val developerProjectLinks: List[DeveloperProjectLink] = List(
    DeveloperProjectLink(1L, 1L),
    DeveloperProjectLink(2L, 1L),
    DeveloperProjectLink(3L, 1L),
    DeveloperProjectLink(5L, 1L),
    DeveloperProjectLink(4L, 2L),
    DeveloperProjectLink(5L, 2L),
    DeveloperProjectLink(6L, 2L),
    DeveloperProjectLink(3L, 2L)
  )

  // task

  val taskTableName = "task"

  val taskTableDefinition: String =
    """|id          BIGSERIAL PRIMARY KEY,
       |label       VARCHAR(255) NOT NULL,
       |description TEXT NOT NULL,
       |deadline    TIMESTAMP NOT NULL,
       |project_id  BIGINT NOT NULL REFERENCES test.project (id) ON UPDATE CASCADE ON DELETE CASCADE
       |""".stripMargin

  val taskCols: List[String] = List("id", "label", "description", "deadline", "project_id")

  val taskColsNoId: List[String] = projectCols.tail

  val tasks: List[Task] = List(
    Task(
      1L,
      "",
      "",
      Timestamp.from(Instant.EPOCH.plusSeconds(3600L * 1)),
      1L
    ),
    Task(
      2L,
      "",
      "",
      Timestamp.from(Instant.EPOCH.plusSeconds(3600L * 1)),
      1L
    ),
    Task(
      3L,
      "",
      "",
      Timestamp.from(Instant.EPOCH.plusSeconds(3600L * 2)),
      2L
    ),
    Task(
      4L,
      "",
      "",
      Timestamp.from(Instant.EPOCH.plusSeconds(3600L * 2)),
      2L
    ),
    Task(
      5L,
      "",
      "",
      Timestamp.from(Instant.EPOCH.plusSeconds(3600L * 3)),
      3L
    ),
    Task(
      6L,
      "",
      "",
      Timestamp.from(Instant.EPOCH.plusSeconds(3600L * 3)),
      3L
    )
  )

  val tasksNoId: List[(String, String, Timestamp, Long)] =
    tasks.map(t => (t.label, t.description, t.deadline, t.projectId))
}
