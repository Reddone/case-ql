package com.github.reddone.caseql.sql

import cats.effect.{ContextShift, IO}
import com.github.reddone.caseql.sql.util.{GenericRepository, TestTransactors}
import com.dimafeng.testcontainers.{ForAllTestContainer, PostgreSQLContainer}
import doobie.util.transactor.Transactor.Aux
import doobie._
import doobie.implicits._
import javasql._
import javatime._
import cats.implicits._
import org.scalatest.Suite
import com.github.reddone.caseql.sql.itmodel.data._
import com.github.reddone.caseql.sql.config.DoobieConfig

import scala.concurrent.ExecutionContext

trait PgSetup { self: Suite with ForAllTestContainer =>

  implicit def ec: ExecutionContext

  implicit val contextShift: ContextShift[IO] = IO.contextShift(ec)

  private var _xa: Aux[IO, Unit] = _

  lazy val xa: Aux[IO, Unit] = _xa

  lazy val container: PostgreSQLContainer = PostgreSQLContainer("postgres:9.6.8")

  override def afterStart(): Unit = {
    val url      = container.jdbcUrl
    val user     = container.username
    val password = container.password

    val config = new DoobieConfig(10, "org.postgresql.Driver", url, user, password) {}
    _xa = TestTransactors.valueOf[IO](config, TestTransactors.BlockerMode.Cached)

    val y = _xa.yolo
    import y._

    val testRepository: GenericRepository = GenericRepository.forSchema(testSchema)

    val initPg = testRepository.createSchema() *>
      testRepository.createTable(developerTableName, developerTableDefinition) *>
      testRepository.createTable(projectTableName, projectTableDefinition) *>
      testRepository.createTable(developerProjectLinkTableName, developerProjectLinkTableDefinition) *>
      testRepository.createTable(taskTableName, taskTableDefinition) *>
      "Finished creating tables".pure[ConnectionIO]

    val populatePg = testRepository.insertMany(developerTableName, developerColsNoId, developersNoId) *>
      testRepository.insertMany(projectTableName, projectColsNoId, projectsNoId) *>
      testRepository.insertMany(developerProjectLinkTableName, developerProjectLinkCols, developerProjectLinks) *>
      testRepository.insertMany(taskTableName, taskColsNoId, tasksNoId) *>
      "Finished populating tables".pure[ConnectionIO]

    (initPg.quick *> populatePg.quick).unsafeRunSync()
  }

  override def beforeStop(): Unit = {}
}
