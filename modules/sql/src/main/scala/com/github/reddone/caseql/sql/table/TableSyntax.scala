package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.util.StringUtils

import scala.language.dynamics

// Syntax decouples a table from its key
final case class TableSyntax[T](alias: String, support: Table[T, _]) extends Dynamic { self =>

  private val nameAlias = if (alias.isEmpty) None else Some(alias)

  private val columnAlias = if (alias.isEmpty) Some(support.name) else Some(alias)

  val name: String = StringUtils.addPrefix(support.name, support.schema)

  val aliasedName: String = StringUtils.addSuffix(name, nameAlias, " ")

  val columns: List[String] = support.fields.map(c).map(StringUtils.addPrefix(_, columnAlias))

  val keyColumns: List[String] = support.keyFields.map(c).map(StringUtils.addPrefix(_, columnAlias))

  def column(field: String): String = StringUtils.addPrefix(c(field), columnAlias)

  def selectDynamic(field: String): String = column(field)

  private def c(field: String): String = support.fieldConverter.getOrElse(field, support.fieldMapper(field))

  def withAlias(newAlias: Option[String]): TableSyntax[T] = newAlias.map(a => copy(alias = a, support)).getOrElse(self)
}

object TableSyntax {

  implicit def derive[T, K](implicit table: Table[T, K]): TableSyntax[T] = table.syntax
}
