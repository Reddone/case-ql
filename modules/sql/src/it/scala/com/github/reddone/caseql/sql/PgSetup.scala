package com.github.reddone.caseql.sql

import cats.effect.{ContextShift, IO}
import com.github.reddone.caseql.sql.util.{GenericRepository, TestTransactors}
import com.dimafeng.testcontainers.{ForAllTestContainer, PostgreSQLContainer}
import com.typesafe.config.ConfigFactory
import doobie.util.transactor.Transactor.Aux
import doobie._
import doobie.implicits._
import javasql._
import javatime._
import cats.implicits._
import org.scalatest.Suite
import ItTestData._
import com.github.reddone.caseql.sql.config.DoobieConfig

import scala.concurrent.ExecutionContext

trait PgSetup { self: Suite with ForAllTestContainer =>

  implicit def ec: ExecutionContext

  implicit val contextShift: ContextShift[IO] = IO.contextShift(ec)

  var _xa: Aux[IO, Unit] = _

  lazy val xa: Aux[IO, Unit] = _xa

  lazy val container: PostgreSQLContainer = PostgreSQLContainer("postgres:9.6.8")

  override def afterStart(): Unit = {
    val url          = container.jdbcUrl
    val user         = container.username
    val password     = container.password
    val config       = ConfigFactory.parseString(s"""|doobie {
                                                 |  numThreads = 10
                                                 |  driverClassName = "org.postgresql.Driver"
                                                 |  url = "$url"
                                                 |  user = "$user"
                                                 |  password = "$password"
                                                 |}""".stripMargin)
    val doobieConfig = DoobieConfig.valueOf(config)
    _xa = TestTransactors.valueOf[IO](doobieConfig, TestTransactors.BlockerMode.Cached)

    val y = _xa.yolo
    import y._

    // TODO: do not use GenericRepository here because it breaks testing rules
    val testRepository: GenericRepository = GenericRepository.forSchema(testSchema)

    val initPg = testRepository.createSchema() *>
      testRepository.createTable(developerTableName, developerTableDefinition) *>
      testRepository.createTable(projectTableName, projectTableDefinition) *>
      testRepository.createTable(developerProjectLinkTableName, developerProjectLinkTableDefinition) *>
      "Finished creating tables".pure[ConnectionIO]

    val populatePg =
      testRepository.insertMany(developerTableName, developerColsNoId, developersNoId) *>
        testRepository.insertMany(projectTableName, projectColsNoId, projectsNoId) *>
        testRepository.insertMany(developerProjectLinkTableName, developerProjectLinkCols, developerProjectLinks) *>
        "Finished populating tables".pure[ConnectionIO]

    (initPg.quick *> populatePg.quick).unsafeRunSync()
  }

  override def beforeStop(): Unit = {}
}
