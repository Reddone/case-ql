package com.github.reddone.caseql.sql.hi

import java.sql.{PreparedStatement, ResultSet}

import cats.implicits._
import com.github.reddone.caseql.sql.util.Raw.Row
import doobie.{ConnectionIO, FC, FPS, FRS, PreparedStatementIO}
import doobie.util.stream.repeatEvalChunks
import fs2.Stream
import fs2.Stream.{bracket, eval}
import resultset._

object connection {

  def liftStreamRaw(
      chunkSize: Int,
      create: ConnectionIO[PreparedStatement],
      prep: PreparedStatementIO[Unit],
      exec: PreparedStatementIO[ResultSet]
  ): Stream[ConnectionIO, Row] = {
    def prepared(ps: PreparedStatement): Stream[ConnectionIO, PreparedStatement] =
      eval[ConnectionIO, PreparedStatement] {
        val fs = FPS.setFetchSize(chunkSize)
        FC.embed(ps, fs *> prep).map(_ => ps)
      }

    def unrolled(rs: ResultSet): Stream[ConnectionIO, Row] =
      repeatEvalChunks(FC.embed(rs, getNextChunkRaw(chunkSize)))

    val preparedStatement: Stream[ConnectionIO, PreparedStatement] =
      bracket(create)(FC.embed(_, FPS.close)).flatMap(prepared)

    def results(ps: PreparedStatement): Stream[ConnectionIO, Row] =
      bracket(FC.embed(ps, exec))(FC.embed(_, FRS.close)).flatMap(unrolled)

    preparedStatement.flatMap(results)
  }

  def streamRaw(sql: String, prep: PreparedStatementIO[Unit], chunkSize: Int): Stream[ConnectionIO, Row] =
    liftStreamRaw(chunkSize, FC.prepareStatement(sql), prep, FPS.executeQuery)
}
