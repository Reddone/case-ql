package com.github.reddone.caseql.example.http

import akka.http.scaladsl.model.HttpMethods.{GET, OPTIONS, POST}
import akka.http.scaladsl.model.headers.{
  `Access-Control-Allow-Credentials`,
  `Access-Control-Allow-Headers`,
  `Access-Control-Allow-Methods`,
  `Access-Control-Allow-Origin`
}
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive0, Route}

trait CorsSupport {

  private def addAccessControlHeaders: Directive0 = {
    respondWithHeaders(
      `Access-Control-Allow-Origin`.*,
      `Access-Control-Allow-Credentials`(true),
      `Access-Control-Allow-Headers`("Authorization", "Content-Type", "X-Requested-With")
    )
  }

  private def preflightRequestHandler: Route = options {
    complete(
      HttpResponse(StatusCodes.OK)
        .withHeaders(`Access-Control-Allow-Methods`(OPTIONS, POST, GET))
    )
  }

  def corsHandler(r: Route): Route = addAccessControlHeaders {
    preflightRequestHandler ~ r
  }
}

object CorsSupport extends CorsSupport
