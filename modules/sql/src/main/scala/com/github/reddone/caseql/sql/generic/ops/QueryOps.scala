package com.github.reddone.caseql.sql.generic.ops

import doobie.{ConnectionIO, Fragment}
import fs2.Stream

object QueryOps {

  trait SQLFragment {
    def toFragment: Fragment
  }

  trait SQLQuery[R] { self: SQLFragment =>
    def execute: ConnectionIO[R]
  }

  trait SQLStreamingQuery[R] { self: SQLFragment =>
    def execute: Stream[ConnectionIO, R]

    def asSQLQuery: SQLQuery[List[R]] = new SQLQuery[List[R]] with SQLFragment {
      def toFragment: Fragment           = self.toFragment
      def execute: ConnectionIO[List[R]] = self.execute.compile.toList
    }
  }
}
