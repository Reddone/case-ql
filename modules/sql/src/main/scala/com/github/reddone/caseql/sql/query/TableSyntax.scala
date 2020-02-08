package com.github.reddone.caseql.sql.query

import com.github.reddone.caseql.sql.util.StringUtils

import scala.language.dynamics

// Syntax decouples a table from its key
final case class TableSyntax[T](alias: String, support: Table[T, _]) extends Dynamic {

  private val aliasO = if (alias.isEmpty) None else Some(alias)

  lazy val name: String = {
    val fullName        = StringUtils.addPrefix(support.name, support.schema)
    val aliasedFullName = StringUtils.addSuffix(fullName, aliasO, " ")
    if (alias == support.name) fullName else aliasedFullName
  }

  lazy val columns: List[String] = support.fields.map(c).map(StringUtils.addPrefix(_, aliasO))

  lazy val keyColumns: List[String] = support.keyFields.map(c).map(StringUtils.addPrefix(_, aliasO))

  def column(field: String): String = StringUtils.addPrefix(c(field), aliasO)

  def selectDynamic(field: String): String = StringUtils.addPrefix(c(field), aliasO)

  private def c(field: String): String = support.fieldConverter.getOrElse(field, support.fieldMapper(field))
}

object TableSyntax {

  implicit def derive[T, K](implicit table: Table[T, K]): TableSyntax[T] = table.internalSyntax
}
