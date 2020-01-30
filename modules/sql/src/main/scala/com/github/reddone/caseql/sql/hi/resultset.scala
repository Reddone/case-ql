package com.github.reddone.caseql.sql.hi

import cats.implicits._
import com.github.reddone.caseql.sql.util.Raw.Row
import doobie._
import doobie.implicits._
import doobie.hi.{FRS, ResultSetIO}

import scala.collection.mutable

object resultset {

  def getNextChunkRaw(chunkSize: Int): ResultSetIO[Seq[Row]] =
    getNextChunkRawV(chunkSize).widen[Seq[Row]]

  def getNextChunkRawV(chunkSize: Int): ResultSetIO[Vector[Row]] =
    FRS.raw { rs =>
      val md = rs.getMetaData
      val lns = (1 to md.getColumnCount)
        .map(i => (md.getColumnLabel(i), md.isNullable(i)))
        .toList
      var n = chunkSize
      val b = Vector.newBuilder[Row]
      while (n > 0 && rs.next) {
        val mb = mutable.LinkedHashMap.newBuilder[String, Any]
        lns.foreach({
          case (label, nullability) =>
            val rsObject = rs.getObject(label)
            val anyValue = if (nullability == 0) { // columnNoNulls
              rsObject
            } else if (nullability == 1) { // columnNullable
              if (rs.wasNull) {
                None
              } else {
                Some(rsObject)
              }
            } else { // columnNullableUnknown
              throw new IllegalArgumentException(s"Cannot infer nullability for column with label $label")
            }
            mb += (label -> anyValue)
        })
        b += mb.result()
        n -= 1
      }
      b.result()
    }
}
