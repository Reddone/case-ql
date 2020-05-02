package com.github.reddone.caseql.example

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import cats.effect._
import cats.implicits._
import com.github.reddone.caseql.example.resource.{ServerResource, TransactorResource}
import com.typesafe.config.{Config, ConfigFactory}
import org.apache.logging.log4j.scala.Logging

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration

object Example extends IOApp with Logging {

  implicit val actorSystem: ActorSystem           = ActorSystem("example-system")
  implicit val executionContext: ExecutionContext = actorSystem.dispatcher

  override implicit val contextShift: ContextShift[IO] = IO.contextShift(executionContext)
  override implicit val timer: Timer[IO]               = IO.timer(executionContext)

  def createServer[F[_]: Effect: ContextShift: Timer](config: Config): Resource[F, Http.ServerBinding] = {
    val serverRoot     = config.getString("server.root")
    val serverDeadline = Duration(config.getDuration("server.deadline").getSeconds, TimeUnit.SECONDS)

    for {
      xa <- TransactorResource[F](config)
      userContext = SangriaContext.production[F](xa)
      server <- ServerResource[F](serverRoot, serverDeadline, userContext)
    } yield server
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val config = ConfigFactory.load()

    createServer[IO](config).use { binding =>
      logger.info(s"Server bound to ${binding.localAddress}")
      IO.never.as(ExitCode.Success)
    }
  }
}
