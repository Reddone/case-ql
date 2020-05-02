package com.github.reddone.caseql.example.resource

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import cats.effect._
import cats.implicits._
import com.github.reddone.caseql.example.{AkkaSangriaServer, SangriaContext, SangriaResolver, SangriaSchema}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

object ServerResource {

  def apply[F[_]: Effect](
      serverRoot: String,
      serverDeadline: FiniteDuration,
      userContext: SangriaContext[F]
  )(
      implicit
      system: ActorSystem,
      ec: ExecutionContext
  ): Resource[F, Http.ServerBinding] = {
    val akkaServer = AkkaSangriaServer[SangriaContext[F]](
      SangriaSchema[F],
      SangriaResolver[F]
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
}
