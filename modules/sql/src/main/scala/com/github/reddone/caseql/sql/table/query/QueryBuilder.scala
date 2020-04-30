package com.github.reddone.caseql.sql.table.query

import com.github.reddone.caseql.sql.table.{Table, TableSyntax}
import com.github.reddone.caseql.sql.tokens.{And, Placeholder}
import doobie._
import fs2.Stream

abstract class QueryBuilder[A, K](table: Table[A, K]) {

  final def byKeyFragment(key: K, alias: String): Fragment =
    table.keyWrite
      .toFragment(
        key,
        table.syntax
          .withAlias(alias)
          .selectionKeyColumns
          .map(col => s"$col = $Placeholder")
          .mkString(s" $And ")
      )
}

trait SqlFragment {
  def toFragment: Fragment
}

trait SqlAction[R] extends SqlFragment {
  def execute: ConnectionIO[R]
}

trait SqlStreamingAction[R] extends SqlFragment { self =>
  def execute: Stream[ConnectionIO, R]

  final def asSqlAction: SqlAction[List[R]] = new SqlAction[List[R]] {
    def toFragment: Fragment           = self.toFragment
    def execute: ConnectionIO[List[R]] = self.execute.compile.toList
  }
}
