package com.github.reddone.caseql.example

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.MediaTypes.`text/html`
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import com.github.reddone.caseql.example.http.CorsSupport
import com.github.reddone.caseql.example.http.GraphQLSupport._
import de.heikoseeberger.akkahttpcirce.ErrorAccumulatingCirceSupport._
import io.circe.Json
import io.circe.optics.JsonPath.root
import io.circe.parser.parse
import sangria.ast.Document
import sangria.execution.deferred.DeferredResolver
import sangria.execution.{ErrorWithResolver, Executor, QueryAnalysisError}
import sangria.marshalling.circe._
import sangria.parser.DeliveryScheme.Try
import sangria.parser.{QueryParser, SyntaxError}
import sangria.schema.Schema

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

trait AkkaServer[Ctx] extends CorsSupport {

  implicit def system: ActorSystem
  implicit def ec: ExecutionContext

  def schema: Schema[Ctx, Unit]
  def deferredResolver: DeferredResolver[Ctx]

  def executeGraphQL(
      query: Document,
      userContext: Ctx,
      operationName: Option[String],
      variables: Json,
      tracing: Boolean
  ): StandardRoute =
    complete(
      Executor
        .execute(
          schema = schema,
          queryAst = query,
          userContext = userContext,
          operationName = operationName,
          variables = if (variables.isNull) Json.obj() else variables,
          deferredResolver = deferredResolver
        )
        .map(OK → _)
        .recover {
          case error: QueryAnalysisError ⇒ BadRequest          → error.resolveError
          case error: ErrorWithResolver  ⇒ InternalServerError → error.resolveError
        }
    )

  def formatError(error: Throwable): Json = error match {
    case syntaxError: SyntaxError ⇒
      Json.obj(
        "errors" → Json.arr(
          Json.obj(
            "message" → Json.fromString(syntaxError.getMessage),
            "locations" → Json.arr(
              Json.obj(
                "line"   → Json.fromBigInt(syntaxError.originalError.position.line),
                "column" → Json.fromBigInt(syntaxError.originalError.position.column)
              )
            )
          )
        )
      )
    case NonFatal(e) ⇒
      formatError(e.getMessage)
    case e ⇒
      throw e
  }

  def formatError(message: String): Json =
    Json.obj("errors" → Json.arr(Json.obj("message" → Json.fromString(message))))

  def startAkkaServer(serverRoot: String, userContext: Ctx): Future[Http.ServerBinding] = {
    val route: Route =
      optionalHeaderValueByName("X-Apollo-Tracing") { tracing ⇒
        redirectToNoTrailingSlashIfPresent(Found) {
          pathPrefix(serverRoot) {
            (get & pathEnd) {
              explicitlyAccepts(`text/html`) {
                getFromResource("assets/playground.html")
              }
            } ~ path("graphql") {
              get {
                parameters(("query", "operationName".?, "variables".?)) {
                  (queryParam, operationNameParam, variablesParam) ⇒
                    QueryParser.parse(queryParam) match {
                      case Success(ast) ⇒
                        variablesParam.map(parse) match {
                          case Some(Left(error)) ⇒
                            complete(BadRequest -> formatError(error))
                          case Some(Right(json)) ⇒
                            executeGraphQL(ast, userContext, operationNameParam, json, tracing.isDefined)
                          case None ⇒
                            executeGraphQL(ast, userContext, operationNameParam, Json.obj(), tracing.isDefined)
                        }
                      case Failure(error) ⇒ complete(BadRequest -> formatError(error))
                    }
                }
              } ~ post {
                parameters(("query".?, "operationName".?, "variables".?)) {
                  (queryParam, operationNameParam, variablesParam) ⇒
                    entity(as[Json]) { body ⇒
                      val query         = queryParam orElse root.query.string.getOption(body)
                      val operationName = operationNameParam orElse root.operationName.string.getOption(body)
                      val variables     = variablesParam orElse root.variables.string.getOption(body)

                      query.map(QueryParser.parse(_)) match {
                        case Some(Success(ast)) ⇒
                          variables.map(parse) match {
                            case Some(Left(error)) ⇒
                              complete(BadRequest -> formatError(error))
                            case Some(Right(json)) ⇒
                              executeGraphQL(ast, userContext, operationName, json, tracing.isDefined)
                            case None ⇒
                              executeGraphQL(ast, userContext, operationName, Json.obj(), tracing.isDefined)
                          }
                        case Some(Failure(error)) ⇒ complete(BadRequest -> formatError(error))
                        case None                 ⇒ complete(BadRequest -> formatError("No query to execute"))
                      }
                    } ~
                      entity(as[Document]) { document ⇒
                        variablesParam.map(parse) match {
                          case Some(Left(error)) ⇒
                            complete(BadRequest -> formatError(error))
                          case Some(Right(json)) ⇒
                            executeGraphQL(document, userContext, operationNameParam, json, tracing.isDefined)
                          case None ⇒
                            executeGraphQL(document, userContext, operationNameParam, Json.obj(), tracing.isDefined)
                        }
                      }
                }
              }
            }
          }
        }
      } ~
        (get & pathEndOrSingleSlash) {
          redirect(s"/$serverRoot", PermanentRedirect)
        } ~
        (get & path("health")) {
          complete {
            "Hey there!"
          }
        }

    val binding = Http().bindAndHandle(
      corsHandler(route),
      "0.0.0.0",
      sys.props.get("http.port").fold(4000)(_.toInt)
    )

    binding
  }
}
