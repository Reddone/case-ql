package com.github.reddone.caseql.sql.hi

import java.sql.{PreparedStatement, ResultSet}

import cats.implicits._
import com.github.reddone.caseql.sql.util.Raw.Row
import doobie._
import doobie.implicits._
import fs2.Stream
import fs2.Stream.{bracket, eval}
import resultset._

// Taken from doobie examples, because using Read[Row] with stream results in many metadata calls. It is not
// a problem on medium sized tables, so you can use Row provided in this project with the normal doobie api.
// This implementation calls getMetadata just one time instead of always calling it on unsafeGet:
// https://github.com/tpolecat/doobie/blob/master/modules/example/src/main/scala/example/GenericStream.scala
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
      util.stream.repeatEvalChunks(FC.embed(rs, getNextChunkRaw(chunkSize)))

    val preparedStatement: Stream[ConnectionIO, PreparedStatement] =
      bracket(create)(FC.embed(_, FPS.close)).flatMap(prepared)

    def results(ps: PreparedStatement): Stream[ConnectionIO, Row] =
      bracket(FC.embed(ps, exec))(FC.embed(_, FRS.close)).flatMap(unrolled)

    preparedStatement.flatMap(results)
  }

  def streamRaw(sql: String, prep: PreparedStatementIO[Unit], chunkSize: Int): Stream[ConnectionIO, Row] =
    liftStreamRaw(chunkSize, FC.prepareStatement(sql), prep, FPS.executeQuery)
}
