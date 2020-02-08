package com.github.reddone.caseql.sql.query.action

import doobie._
import fs2.Stream

object QueryAction {

  trait SQLFragment {
    def toFragment: Fragment
  }

  trait SQLAction[R] { self: SQLFragment =>
    def execute: ConnectionIO[R]
  }

  trait SQLStreamingAction[R] { self: SQLFragment =>
    def execute: Stream[ConnectionIO, R]

    final def asSQLAction: SQLAction[List[R]] = new SQLAction[List[R]] with SQLFragment {
      def toFragment: Fragment           = self.toFragment
      def execute: ConnectionIO[List[R]] = self.execute.compile.toList
    }
  }
}
