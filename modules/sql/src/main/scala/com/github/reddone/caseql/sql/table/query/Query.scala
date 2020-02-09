package com.github.reddone.caseql.sql.table.query

import doobie._
import fs2.Stream

object Query {

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
}
