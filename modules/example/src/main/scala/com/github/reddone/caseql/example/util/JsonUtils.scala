package com.github.reddone.caseql.example.util

import io.circe.{Json, parser}

object JsonUtils {

  def unsafeParseJson(input: String): Json = {
    parser.parse(input).getOrElse(throw new IllegalArgumentException("wrong json"))
  }
}
