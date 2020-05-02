package com.github.reddone.caseql.example

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import cats.effect._
import cats.implicits._
import com.github.reddone.caseql.example.resource.TransactorResource
import com.typesafe.config.{Config, ConfigFactory}
import org.apache.logging.log4j.scala.Logging

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.{Failure, Success}

object Example extends IOApp with Logging {

  implicit val system: ActorSystem                = ActorSystem("example-system")
  implicit val executionContext: ExecutionContext = system.dispatcher

  override implicit val contextShift: ContextShift[IO] = IO.contextShift(executionContext)
  override implicit val timer: Timer[IO]               = IO.timer(executionContext)

  def serverResource[F[_]: Effect](
      serverRoot: String,
      serverDeadline: FiniteDuration,
      userContext: SangriaContext[F]
  ): Resource[F, Http.ServerBinding] = {
    val akkaServer = AkkaServer[SangriaContext[F]](
      SangriaSchema.schema[F],
      SangriaSchema.deferredResolver[F]
    )
    val alloc = Async[F].async[Http.ServerBinding] { cb =>
      akkaServer.start(serverRoot, userContext).onComplete { r =>
        cb(r match {
          case Success(binding) => Right(binding)
          case Failure(ex)      => Left(ex)
        })
      }
    }
    val free = (binding: Http.ServerBinding) =>
      Async[F].async[Http.HttpTerminated] { cb =>
        binding.terminate(serverDeadline).onComplete { r =>
          cb(r match {
            case Success(terminated) => Right(terminated)
            case Failure(ex)         => Left(ex)
          })
        }
      }

    Resource.make(alloc)(free(_).void)
  }

  def createServer[F[_]: Effect: ContextShift: Timer](config: Config): Resource[F, Http.ServerBinding] = {
    val serverRoot     = config.getString("server.root")
    val serverDeadline = Duration(config.getDuration("server.deadline").getSeconds, TimeUnit.SECONDS)

    for {
      xa <- TransactorResource[F](config)
      userContext = SangriaContext.production[F](xa)
      server <- serverResource[F](serverRoot, serverDeadline, userContext)
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
