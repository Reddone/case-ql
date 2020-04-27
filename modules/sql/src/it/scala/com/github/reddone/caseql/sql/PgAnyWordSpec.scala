package com.github.reddone.caseql.sql

import cats.effect.IO
import com.dimafeng.testcontainers.ForAllTestContainer
import doobie.scalatest.IOAnalysisMatchers
import doobie.util.transactor.Transactor
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.ExecutionContext

abstract class PgAnyWordSpec
    extends AnyWordSpec
    with Matchers
    with ScalaFutures
    with IOAnalysisMatchers
    with ForAllTestContainer
    with PgSetup {

  override implicit def ec: ExecutionContext = ExecutionContext.Implicits.global

  override def transactor: Transactor[IO] = xa
}
