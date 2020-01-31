package com.github.reddone.caseql.sql.util

import java.sql.{ParameterMetaData, PreparedStatement, ResultSet, ResultSetMetaData}

import cats.implicits._
import com.github.reddone.caseql.sql.hi.connection
import doobie._
import doobie.implicits._
import fs2.Stream

import scala.collection.mutable

object Raw {

  type Row = mutable.LinkedHashMap[String, Any] // order matters when doing writes

  private final case class ColMetadata(
      className: String,
      label: String,
      name: String,
      tpe: Int,
      tpeName: String,
      precision: Int,
      scale: Int,
      nullability: Int
  )

  private object ColMetadata {

    def of(rsm: ResultSetMetaData, index: Int): ColMetadata = {
      ColMetadata(
        rsm.getColumnClassName(index),
        rsm.getColumnLabel(index),
        rsm.getColumnName(index),
        rsm.getColumnType(index),
        rsm.getColumnTypeName(index),
        rsm.getPrecision(index),
        rsm.getScale(index),
        rsm.isNullable(index)
      )
    }
  }

  private final case class ParamMetadata(
      className: String,
      tpe: Int,
      tpeName: String,
      precision: Int,
      scale: Int,
      nullability: Int
  )

  private object ParamMetadata {

    def of(pm: ParameterMetaData, index: Int): ParamMetadata = {
      ParamMetadata(
        pm.getParameterClassName(index),
        pm.getParameterType(index),
        pm.getParameterTypeName(index),
        pm.getPrecision(index),
        pm.getScale(index),
        pm.isNullable(index)
      )
    }
  }

  def processRaw(sql: String, chunkSize: Int = 100): Stream[ConnectionIO, Row] =
    connection.streamRaw(sql, ().pure[PreparedStatementIO], chunkSize)

  implicit val rowRead: Read[Row] = new Read[Row](Nil, unsafeGet)

  implicit val rowWrite: Write[Row] = new Write[Row](Nil, _.values.toList, unsafeSet, unsafeUpdate)

  private def unsafeGet(rs: ResultSet, n: Int): Row = {
    assert(n > 0, "Row unsafeGet column index must be greater than 0")
    val rsMeta         = rs.getMetaData
    val colCount       = rsMeta.getColumnCount
    val colMetaSeq     = (n to colCount).map(ColMetadata.of(rsMeta, _))
    val mutableBuilder = mutable.LinkedHashMap.newBuilder[String, Any]
    colMetaSeq.foreach({
      case ColMetadata(_, label, _, _, _, _, _, nullability) =>
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
    mutableBuilder.result()
  }

  private def unsafeSet(ps: PreparedStatement, n: Int, a: Row): Unit = {
    assert(n > 0, "Row unsafeSet column index must be greater than 0")
    val psMeta     = ps.getParameterMetaData
    val paramCount = psMeta.getParameterCount
    if (paramCount - n + 1 != a.size) {
      throw new IllegalArgumentException(
        s"You are trying to set ${paramCount - n + 1} parameters using ${a.size} values"
      )
    }
    val paramMetaWithIndexSeq = (n to paramCount).map(i => (i, ParamMetadata.of(psMeta, i)))
    a.valuesIterator.toList // respect insertion order
      .zip(paramMetaWithIndexSeq)
      .foreach({
        case (value, (index, ParamMetadata(_, tpe, _, _, _, _))) =>
          value match {
            case _: None.type => ps.setNull(index, tpe)
            case v: Some[_]   => ps.setObject(index, v.get, tpe)
            case v            => ps.setObject(index, v, tpe)
          }
      })
  }

  private def unsafeUpdate(rs: ResultSet, n: Int, a: Row): Unit = {
    assert(n > 0, "Row unsafeUpdate column index must be greater than 0")
    val rsMeta   = rs.getMetaData
    val colCount = rsMeta.getColumnCount
    if (colCount - n + 1 != a.size) {
      throw new IllegalArgumentException(
        s"You are trying to update ${colCount - n + 1} columns using ${a.size} values"
      )
    }
    val colMetaSeq = (n to colCount).map(ColMetadata.of(rsMeta, _))
    colMetaSeq.foreach({
      case ColMetadata(_, label, _, _, _, _, _, _) =>
        val value = a.getOrElse(label, throw new IllegalArgumentException(s"Cannot get Row value for column $label"))
        value match {
          case _: None.type => rs.updateNull(label)
          case v: Some[_]   => rs.updateObject(label, v.get)
          case v            => rs.updateObject(label, v)
        }
    })
  }
}
