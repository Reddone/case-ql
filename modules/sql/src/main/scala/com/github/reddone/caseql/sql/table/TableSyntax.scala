package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.util.StringUtils

final case class TableSyntax[A](alias: String, support: Table[A, _]) {

  val name: String = if (alias.isEmpty) support.name else alias

  val fullName: String = StringUtils.addSuffix(
    StringUtils.addPrefix(
      support.name,
      support.schema
    ),
    if (alias.isEmpty) None else Some(alias),
    " "
  )

  val columns: List[String] = support.fields.map(column)

  val selectionColumns: List[String] = support.fields.map(selectionColumn)

  val keyColumns: List[String] = support.keyFields.map(column)

  val selectionKeyColumns: List[String] = support.keyFields.map(selectionColumn)

  def column(field: String): String = c(field)

  def selectionColumn(field: String): String = StringUtils.addPrefix(
    c(field),
    Some(name)
  )

  def withAlias(newAlias: String): TableSyntax[A] = copy(alias = newAlias)

  private def c(field: String): String = support.fieldConverter.getOrElse(field, support.fieldMapper(field))
}

object TableSyntax {

  implicit def derive[A, K](implicit table: Table[A, K]): TableSyntax[A] = table.syntax
}
