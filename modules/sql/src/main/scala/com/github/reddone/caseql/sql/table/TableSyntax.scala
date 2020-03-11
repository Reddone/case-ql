package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.util.StringUtils

final case class TableSyntax[A](alias: String, support: Table[A, _]) {

  val name: String = StringUtils.addPrefix(
    support.name,
    support.schema
  )

  val aliasedName: String = StringUtils.addSuffix(
    name,
    if (alias.isEmpty) None else Some(alias),
    " "
  )

  val columns: List[String] = support.fields.map(column)

  val aliasedColumns: List[String] = support.fields.map(aliasedColumn) // used for select and where

  val keyColumns: List[String] = support.keyFields.map(column)

  val aliasedKeyColumns: List[String] = support.keyFields.map(aliasedColumn) // used for select and where

  def column(field: String): String = c(field)

  def aliasedColumn(field: String): String = StringUtils.addPrefix(
    c(field),
    if (alias.isEmpty) Some(support.name) else Some(alias)
  )

  def withAlias(newAlias: String): TableSyntax[A] = copy(alias = newAlias)

  private def c(field: String): String = support.fieldConverter.getOrElse(field, support.fieldMapper(field))
}

object TableSyntax {

  implicit def derive[A, K](implicit table: Table[A, K]): TableSyntax[A] = table.syntax
}
