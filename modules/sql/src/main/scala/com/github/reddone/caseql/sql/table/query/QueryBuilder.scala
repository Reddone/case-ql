package com.github.reddone.caseql.sql.table.query

import com.github.reddone.caseql.sql.table.{Table, TableSyntax}
import com.github.reddone.caseql.sql.tokens.{And, Placeholder}
import doobie._
import fs2.Stream

abstract class QueryBuilder[A, K](table: Table[A, K], alias: Option[String]) {

  final val querySyntax: TableSyntax[A] = alias.map(table.syntax.withAlias).getOrElse(table.syntax)

  final def byKeyFragment(key: K): Fragment =
    table.keyWrite
      .toFragment(
        key,
        querySyntax.aliasedKeyColumns
          .map(col => s"$col = $Placeholder")
          .mkString(s" $And ")
      )

}

trait SQLFragment {
  def toFragment: Fragment
}

trait SQLAction[R] extends SQLFragment {
  def execute: ConnectionIO[R]
}

trait SQLStreamingAction[R] extends SQLFragment { self =>
  def execute: Stream[ConnectionIO, R]

  final def asSQLAction: SQLAction[List[R]] = new SQLAction[List[R]] {
    def toFragment: Fragment           = self.toFragment
    def execute: ConnectionIO[List[R]] = self.execute.compile.toList
  }
}
