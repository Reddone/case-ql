package com.github.reddone.caseql.sql.util

import java.sql.{ParameterMetaData, PreparedStatement, ResultSet, ResultSetMetaData}

import doobie._

import scala.collection.mutable

object Raw {

  type Row = mutable.LinkedHashMap[String, Any]

  implicit val rowRead: Read[Row] = new Read[Row](Nil, unsafeGet)

  implicit val rowWrite: Write[Row] = new Write[Row](Nil, _.values.toList, unsafeSet, unsafeUpdate)

  private case class ColMetadata(
      className: String,
      label: String,
      name: String,
      tpe: Int,
      tpeName: String,
      precision: Int,
      scale: Int,
      isNullable: Int
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

  private case class ParamMetadata(
      className: String,
      tpe: Int,
      tpeName: String,
      precision: Int,
      scale: Int,
      isNullable: Int
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

  private def unsafeGet(rs: ResultSet, n: Int): Row = {
    assert(n > 0, "unsafeGet column index must be greater than 0")
    val metadata = rs.getMetaData
    val columnLabelsAndNullabilities = (n to metadata.getColumnCount)
      .map(i => (metadata.getColumnLabel(i), metadata.isNullable(i)))
      .toList
    val builder = mutable.LinkedHashMap.newBuilder[String, Any]
    columnLabelsAndNullabilities.foreach({
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
        builder += (label -> anyValue)
    })
    builder.result()
  }

  private def unsafeSet(ps: PreparedStatement, n: Int, a: Row): Unit = {
    assert(n > 0, "UnsafeSet column index must be greater than 0")
    val metadata       = ps.getParameterMetaData
    val parameterCount = metadata.getParameterCount
    if (parameterCount - n + 1 < a.size) {
      throw new IllegalArgumentException(
        s"You are trying to set ${parameterCount - n + 1} parameters using ${a.size} values"
      )
    }
    val parameterIndicesAndTypes = (n to parameterCount)
      .map(n => (n, metadata.getParameterType(n)))
      .toList
    println((n to parameterCount).map(ParamMetadata.of(metadata, _)))
    a.valuesIterator.toList
      .zip(parameterIndicesAndTypes)
      .foreach({
        case (anyValue, (index, sqlType)) =>
          anyValue match {
            case _: None.type => ps.setNull(index, sqlType)
            case v: Some[_]   => ps.setObject(index, v.get, sqlType)
            case v            => ps.setObject(index, v, sqlType)
          }
      })
  }

  // TODO: insert nullability check using metadata
  private def unsafeUpdate(rs: ResultSet, n: Int, a: Row): Unit = {
    assert(n > 0, "unsafeUpdate column index must be greater than 0")
    val metadata = rs.getMetaData
    val columnLabels = (n to metadata.getColumnCount)
      .map(i => metadata.getColumnLabel(i))
      .toList
    columnLabels.foreach({ label =>
      val anyValue = a.getOrElse(label, throw new IllegalArgumentException(s"Cannot get map value at key $label"))
      anyValue match {
        case _: None.type => rs.updateNull(label)
        case v: Some[_]   => rs.updateObject(label, v.get)
        case v            => rs.updateObject(label, v)
      }
    })
  }
}
