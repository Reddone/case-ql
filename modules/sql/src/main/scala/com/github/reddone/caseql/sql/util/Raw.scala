package com.github.reddone.caseql.sql.util

import java.sql.{PreparedStatement, ResultSet}

import doobie._

import scala.collection.mutable

object Raw {

  type Row = mutable.LinkedHashMap[String, Any]

  implicit val rowRead: Read[Row] = new Read[Row](Nil, unsafeGet)

  implicit val rowWrite: Write[Row] = new Write[Row](Nil, _.values.toList, unsafeSet, unsafeUpdate)

  private def unsafeGet(rs: ResultSet, n: Int): Row = {
    assert(n > 0, "UnsafeGet column index must be greater than 0")
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
    val parameterIndicesAndTypes = (n to parameterCount).map(n => (n, metadata.getParameterType(n))).toList
    a.valuesIterator.toList
      .zip(parameterIndicesAndTypes)
      .foreach({
        case (anyValue, (index, sqlType)) =>
          anyValue match {
            case _: None.type => ps.setNull(index, sqlType)
            case v: Some[_]   => ps.setObject(index, v.get)
            case v            => ps.setObject(index, v)
          }
      })
  }

  private def unsafeUpdate(rs: ResultSet, n: Int, a: Row): Unit = {
    assert(n > 0, "UnsafeUpdate column index must be greater than 0")
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
