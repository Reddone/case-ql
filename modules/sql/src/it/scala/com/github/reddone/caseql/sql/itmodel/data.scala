package com.github.reddone.caseql.sql.itmodel

import java.sql.Timestamp
import java.time.Instant

import com.github.reddone.caseql.sql.itmodel.db._

object data {

  val testSchema: String = "public"

  // developer

  val developerTableName: String = "developer"

  val developerTableDefinition: String =
    s"""|id             BIGSERIAL PRIMARY KEY,
        |name           VARCHAR(255) NOT NULL,
        |age            INTEGER NOT NULL,
        |team_leader_id BIGINT NULL REFERENCES $testSchema.developer (id) ON UPDATE CASCADE ON DELETE SET NULL
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
    Project(1L, "Topdeckin N' Wreckin", Some("Kripp most loved hobby")),
    Project(2L, "Random Pasta", None),
    Project(3L, "Welcome to Summoner Rift", Some("Fedding like there's no tomorrow"))
  )

  val projectsNoId: List[(String, Option[String])] =
    projects.map(p => (p.title, p.description))

  // developer_project_link

  val developerProjectLinkTableName: String = "developer_project_link"

  val developerProjectLinkTableDefinition: String =
    s"""|developer_id BIGINT NOT NULL REFERENCES $testSchema.developer (id) ON UPDATE CASCADE ON DELETE CASCADE,
        |project_id   BIGINT NOT NULL REFERENCES $testSchema.project (id) ON UPDATE CASCADE ON DELETE CASCADE
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
    s"""|id          BIGSERIAL PRIMARY KEY,
        |label       VARCHAR(255) NOT NULL,
        |description TEXT NULL DEFAULT 'EAZY PEAZY',
        |deadline    TIMESTAMP NOT NULL,
        |project_id  BIGINT NOT NULL REFERENCES $testSchema.project (id) ON UPDATE CASCADE ON DELETE CASCADE
        |""".stripMargin

  val taskCols: List[String] = List("id", "label", "description", "deadline", "project_id")

  val taskColsNoId: List[String] = taskCols.tail

  val tasks: List[Task] = List(
    Task(
      1L,
      "POC of Top Decking script",
      Some("Description of what to do is deducible with ease from the task label"),
      Timestamp.from(Instant.EPOCH.plusSeconds(3600L * 1)),
      1L
    ),
    Task(
      2L,
      "Implementation of Top Decking script",
      Some("Description of what to do is deducible with ease from the task label"),
      Timestamp.from(Instant.EPOCH.plusSeconds(3600L * 1)),
      1L
    ),
    Task(
      3L,
      "Copy pasta some random text ",
      None,
      Timestamp.from(Instant.EPOCH.plusSeconds(3600L * 2)),
      2L
    ),
    Task(
      4L,
      "Super automated copy pasta with donger bot",
      None,
      Timestamp.from(Instant.EPOCH.plusSeconds(3600L * 2)),
      2L
    ),
    Task(
      5L,
      "First blood or ragequit at 15",
      Some("Description of what to do is deducible with ease from the task label"),
      Timestamp.from(Instant.EPOCH.plusSeconds(3600L * 3)),
      3L
    ),
    Task(
      6L,
      "Report all teammates",
      None,
      Timestamp.from(Instant.EPOCH.plusSeconds(3600L * 3)),
      3L
    )
  )

  val tasksNoId: List[(String, Option[String], Timestamp, Long)] =
    tasks.map(t => (t.label, t.description, t.deadline, t.projectId))
}
