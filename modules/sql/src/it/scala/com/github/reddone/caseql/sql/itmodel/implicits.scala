package com.github.reddone.caseql.sql.itmodel

import com.github.reddone.caseql.sql.itmodel.db._
import com.github.reddone.caseql.sql.table.TableLink.Aux
import com.github.reddone.caseql.sql.table._
import doobie.implicits._
import javasql._
import javatime._

object implicits {

  val useAlias = false

  // TABLE

  implicit val developerTable: Table[Developer, DeveloperKey] =
    Table.derive[Developer, DeveloperKey](useTableAlias = useAlias)

  implicit val projectTable: Table[Project, ProjectKey] =
    Table.derive[Project, ProjectKey](useTableAlias = useAlias)

  implicit val dplTable: Table[DeveloperProjectLink, DeveloperProjectLinkKey] =
    Table.derive[DeveloperProjectLink, DeveloperProjectLinkKey](useTableAlias = useAlias)

  implicit val taskTable: Table[Task, TaskKey] =
    Table.derive[Task, TaskKey](useTableAlias = useAlias)

  // LINK

  implicit val developerSelfLink: Aux[Developer, Developer, Unit] =
    TableLink.self[Developer](
      FieldSet("id"),
      FieldSet("teamLeaderId")
    )

  implicit val developerDplLink: Aux[Developer, DeveloperProjectLink, Unit] =
    TableLink.direct[Developer, DeveloperProjectLink](
      FieldSet("id"),
      FieldSet("developerId")
    )

  implicit val projectDplLink: Aux[Project, DeveloperProjectLink, Unit] =
    TableLink.direct[Project, DeveloperProjectLink](
      FieldSet("id"),
      FieldSet("projectId")
    )

  implicit val taskLink: Aux[Project, Task, Unit] =
    TableLink.direct[Project, Task](
      FieldSet("id"),
      FieldSet("projectId")
    )

  implicit val developerProjectLink: Aux[Developer, Project, DeveloperProjectLink] =
    TableLink.union(developerDplLink, projectDplLink)

  // FILTER

  implicit lazy val developerFilter: TableFilter[Developer, DeveloperFilter] =
    TableFilter.derive[Developer, DeveloperFilter]()

  implicit val projectFilter: TableFilter[Project, ProjectFilter] =
    TableFilter.derive[Project, ProjectFilter]()

  implicit val dplFilter: TableFilter[DeveloperProjectLink, DeveloperProjectLinkFilter] =
    TableFilter.derive[DeveloperProjectLink, DeveloperProjectLinkFilter]()

  implicit val taskFilter: TableFilter[Task, TaskFilter] =
    TableFilter.derive[Task, TaskFilter]()

  // MODIFIER

  implicit val developerModifier: TableModifier[Developer, DeveloperModifier] =
    TableModifier.derive[Developer, DeveloperModifier]()

  implicit val projectModifier: TableModifier[Project, ProjectModifier] =
    TableModifier.derive[Project, ProjectModifier]()

  implicit val dplModifier: TableModifier[DeveloperProjectLink, DeveloperProjectLinkModifier] =
    TableModifier.derive[DeveloperProjectLink, DeveloperProjectLinkModifier]()

  implicit val taskModifier: TableModifier[Task, TaskModifier] =
    TableModifier.derive[Task, TaskModifier]()
}
