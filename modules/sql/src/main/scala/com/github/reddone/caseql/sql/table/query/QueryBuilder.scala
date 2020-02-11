package com.github.reddone.caseql.sql.table.query

import com.github.reddone.caseql.sql.table.{Table, TableSyntax}
import com.github.reddone.caseql.sql.tokens.{And, Placeholder}
import doobie._
import fs2.Stream

abstract class QueryBuilder[T, K](table: Table[T, K], alias: Option[String]) {

  protected val querySyntax: TableSyntax[T] = table.syntax.withAlias(alias)

  final def byKeyFragment(key: K): Fragment = {
    table.keyWrite.toFragment(key, querySyntax.keyColumns.map(col => s"$col = $Placeholder").mkString(s" $And "))
  }
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
