package com.github.reddone.caseql.sql.hi

import cats.implicits._
import com.github.reddone.caseql.sql.util.Raw.Row
import doobie.implicits._
import doobie.hi.{FRS, ResultSetIO}

import scala.collection.mutable

// Taken from doobie examples, because using Read[Row] with stream results in many metadata calls. It is not
// a problem on medium sized tables, so you can use Row provided in this project with the normal doobie api.
// This implementation calls getMetadata just one time instead of always calling it on unsafeGet:
// https://github.com/tpolecat/doobie/blob/master/modules/example/src/main/scala/example/GenericStream.scala
object resultset {

  def getNextChunkRaw(chunkSize: Int): ResultSetIO[Seq[Row]] =
    getNextChunkRawV(chunkSize).widen[Seq[Row]]

  def getNextChunkRawV(chunkSize: Int): ResultSetIO[Vector[Row]] =
    FRS.raw { rs =>
      val rsMeta     = rs.getMetaData
      val colMetaSeq = (1 to rsMeta.getColumnCount).map(i => (rsMeta.getColumnLabel(i), rsMeta.isNullable(i)))
      var n          = chunkSize
      val builder    = Vector.newBuilder[Row]
      while (n > 0 && rs.next) {
        val mutableBuilder = mutable.LinkedHashMap.newBuilder[String, Any]
        colMetaSeq.foreach({
          case (label, nullability) =>
            val rsObject = rs.getObject(label)
            val value = if (nullability == 0) { // columnNoNulls
              rsObject
            } else if (nullability == 1) { // columnNullable
              if (rs.wasNull) {
                None
              } else {
                Some(rsObject)
              }
            } else { // columnNullableUnknown
              throw new IllegalArgumentException(s"Cannot infer nullability for column $label")
            }
            mutableBuilder += (label -> value)
        })
        builder += mutableBuilder.result()
        n -= 1
      }
      builder.result()
    }
}
