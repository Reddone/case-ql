package com.github.reddone.caseql.example

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import cats.effect._
import cats.implicits._
import com.github.reddone.caseql.example.resource.TransactorResource
import com.typesafe.config.{Config, ConfigFactory}
import doobie.util.transactor.Transactor
import org.apache.logging.log4j.scala.Logging
import sangria.execution.deferred.{DeferredResolver, Fetcher}
import sangria.schema.Schema

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.{Failure, Success}

object Example extends IOApp with AkkaServer[SangriaContext] with SangriaSchema with Logging {

  override implicit val system: ActorSystem  = ActorSystem("example-system")
  override implicit val ec: ExecutionContext = system.dispatcher

  override implicit val contextShift: ContextShift[IO] = IO.contextShift(ec)
  override implicit val timer: Timer[IO]               = IO.timer(ec)

  override val schema: Schema[SangriaContext, Unit] = Schema(
    query = Query,
    mutation = Some(Mutation),
    subscription = None,
    additionalTypes = Nil
  )

  override val deferredResolver: DeferredResolver[SangriaContext] = {
    val fetchers = Seq.empty[Fetcher[SangriaContext, _, _, _]]
    DeferredResolver.fetchers(fetchers: _*)
  }

  def createServer[F[_]: Async](xa: Transactor[F], deadline: FiniteDuration): Resource[F, Http.ServerBinding] = {
    val userContext = SangriaContext.production[F](xa)
    val alloc = Async[F].async[Http.ServerBinding] { cb =>
      startAkkaServer("example", userContext).onComplete { r =>
        cb(r match {
          case Success(binding) => Right(binding)
          case Failure(ex)      => Left(ex)
        })
      }
    }
    val free = (binding: Http.ServerBinding) =>
      Async[F].async[Http.HttpTerminated] { cb =>
        binding.terminate(deadline).onComplete { r =>
          cb(r match {
            case Success(terminated) => Right(terminated)
            case Failure(ex)         => Left(ex)
          })
        }
      }
    Resource.make(alloc)(free(_).void)
  }

  def runServer[F[_]: Async: ContextShift: Timer](config: Config): Resource[F, Http.ServerBinding] = {
    val deadline = Duration(config.getDuration("server.hardDeadline").getSeconds, TimeUnit.SECONDS)

    for {
      xa     <- TransactorResource.create[F](config)
      server <- createServer(xa, deadline)
    } yield server
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val config: Config = ConfigFactory.load()

    runServer[IO](config).use { binding =>
      logger.info(s"Server bound to ${binding.localAddress}")
      IO.never.as(ExitCode.Success)
    }
  }
}
