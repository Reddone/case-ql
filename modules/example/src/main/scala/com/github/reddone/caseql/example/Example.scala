package com.github.reddone.caseql.example

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import cats.effect.{ConcurrentEffect, ContextShift, ExitCode, IO, IOApp, Resource, Timer}
import cats.implicits._
import com.github.reddone.caseql.example.repository.TransactorResource
import com.typesafe.config.{Config, ConfigFactory}
import org.apache.logging.log4j.scala.Logging
import sangria.execution.deferred.{DeferredResolver, Fetcher}
import sangria.schema.Schema

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

object Example extends IOApp with AkkaServer[SangriaContext] with SangriaSchema with Logging {

  override implicit val system: ActorSystem = ActorSystem("example-system")

  override implicit val ec: ExecutionContext = system.dispatcher

  override implicit val contextShift: ContextShift[IO] = IO.contextShift(ec)

  override implicit val timer: Timer[IO] = IO.timer(ec)

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

  val config: Config = ConfigFactory.load()

  def serverResource[F[_]: ConcurrentEffect: ContextShift: Timer](
      config: Config
  ): Resource[F, F[Http.ServerBinding]] = {
    val xaResource = TransactorResource.create[F](config)

    val resources = for {
      xa <- xaResource
    } yield (xa)

    resources.map {
      case (xa) =>
        val serverRoot  = "example"
        val userContext = SangriaContext.production[F](xa)
        ConcurrentEffect[F].async[Http.ServerBinding] { cb =>
          startAkkaServer(serverRoot, userContext).onComplete {
            case Success(serverBinding) => cb(Right(serverBinding))
            case Failure(ex)            => cb(Left(ex))
          }
        }
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    serverResource[IO](config)
      .use(_.flatMap { binding =>
        logger.info(s"Server bound to ${binding.localAddress}")
        IO.never.as(ExitCode.Success)
      })
  }
}
