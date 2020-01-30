package com.github.reddone.caseql.sql.hi

import cats.implicits._
import com.github.reddone.caseql.sql.util.Raw.Row
import doobie._
import doobie.implicits._
import fs2.Stream

object RawStream {

  def processRaw(sql: String, chunkSize: Int = 100): Stream[ConnectionIO, Row] =
    connection.streamRaw(sql, ().pure[PreparedStatementIO], chunkSize)
}
