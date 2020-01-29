package com.github.reddone.caseql.sql

import cats.effect.{ContextShift, IO}
import com.github.reddone.caseql.sql.repository.GenericRepository
import com.github.reddone.caseql.sql.util.TestTransactors
import com.dimafeng.testcontainers.{ForAllTestContainer, PostgreSQLContainer}
import com.typesafe.config.ConfigFactory
import doobie.util.transactor.Transactor.Aux
import doobie._
import doobie.implicits._
import cats.implicits._
import org.scalatest.Suite

import scala.concurrent.ExecutionContext

trait PgSetup { self: Suite with ForAllTestContainer =>

  implicit def ec: ExecutionContext

  implicit val contextShift: ContextShift[IO] = IO.contextShift(ec)

  var xa: Aux[IO, Unit] = _

  val container: PostgreSQLContainer = PostgreSQLContainer("postgres:9.6.8")

  val commonRepository: GenericRepository = GenericRepository.forSchema("public")

  override def afterStart(): Unit = {
    val url      = container.jdbcUrl
    val user     = container.username
    val password = container.password
    val config   = ConfigFactory.parseString(s"""|doobie {
                                               |  numThreads = 20
                                               |  driverClassName = "org.postgresql.Driver"
                                               |  url = "$url"
                                               |  user = "$user"
                                               |  password = "$password"
                                               |}""".stripMargin)
    xa = TestTransactors.valueOf[IO](config, TestTransactors.BlockerMode.Cached)

    val y = xa.yolo
    import y._

//    val mediaDDL =
//      commonRepository.createSchemaIfNotExists("media") *>
//        commonRepository.createTableIfNotExists(bc_table, bc_definition) *>
//        commonRepository.createTableIfNotExists(b_table, b_definition) *>
//        "Finished creating tables".pure[ConnectionIO]
//
//    val bc_insert_fields: List[String] = List(bc_label, bc_createdAt, bc_updatedAt)
//    val b_insert_fields: List[String]  = List(b_label, b_categoryId, b_createdAt, b_updatedAt)
//
//    val mediaData =
//      commonRepository.insertMany(bc_table, bc_insert_fields, TestData.brandCategoriesNoId) *>
//        commonRepository.insertMany(b_table, b_insert_fields, TestData.brandsNoId) *>
//        "Finished populating tables".pure[ConnectionIO]
//
//    (mediaDDL.quick *> mediaData.quick).unsafeRunSync()
  }

  override def beforeStop(): Unit = {}
}
