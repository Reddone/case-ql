package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.util.StringUtils

import scala.language.dynamics

final case class TableSyntax[A](alias: Option[String], support: Table[A, _]) extends Dynamic { self =>

  val name: String = StringUtils.addPrefix(support.name, support.schema)

  val aliasedName: String = StringUtils.addSuffix(name, alias, " ")

  val columns: List[String] = support.fields.map(column) // TODO: we need columns and where columns

  val keyColumns: List[String] = support.keyFields.map(column)

  def column(field: String): String = StringUtils.addPrefix(c(field), alias)

  def selectDynamic(field: String): String = column(field)

  private def c(field: String): String = support.fieldConverter.getOrElse(field, support.fieldMapper(field))

  def withAlias(newAlias: Option[String]): TableSyntax[A] = copy(alias = newAlias)
}

object TableSyntax {

  implicit def derive[A, K](implicit table: Table[A, K]): TableSyntax[A] = table.syntax
}
