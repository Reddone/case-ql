//package com.github.reddone.caseql.sql.generic
//
//import cats.implicits._
//import com.github.reddone.caseql.sql.filter.EntityFilter
//import doobie._
//import doobie.implicits._
//import Fragment._
//import cats.free.Free
//import com.github.reddone.caseql.sql.util.FragmentUtils
//import doobie.free.connection
//import fs2.Stream
//

// x11 join xx11
//
//
//
//

//case class Selection[T]() {
//
//
//  def whereRelation[B, A](filter: EntityFilter[B, A])(implicit link: Link[T, B])
//}
//object Query {
//
//  trait Selection
//
//  trait Mutation
//
//  trait Operation[T, R] {
//    def fragment: Fragment
//    def execute: ConnectionIO[R]
//  }
//
//  class Selector[T] extends Operation[T, List[T]] {
//    def fragment: Fragment = ???
//
//    def execute: ConnectionIO[List[T]] = ???
//  }
//
//  class Inserter[T] extends Operation {
//    def fragment: Fragment = ???
//
//    def execute: ConnectionIO[Int] = ???
//  }
//
//  class InserterReturningKeys[T, U](modifier: U)(
//      implicit
//      table: Table[T],
//      tableModifier: TableModifier[T, U]
//  ) {
//    def fragment: Fragment = ???
//
//    def execute: Stream[ConnectionIO, table.Key] = ???
//  }
//
//  class Updater[T, U, V <: EntityFilter[V]](modifier: U, filter: V)(
//      implicit
//      table: Table[T],
//      tableFilter: TableFilter[T, V],
//      tableModifier: TableModifier[T, U]
//  ) {
//    def fragment: Fragment = ???
//
//    def execute: ConnectionIO[Int] = ???
//  }
//
//  object Updater {
//
//    def multi[T, U, V <: EntityFilter[V]](updaters: List[Updater[T, U, V]]): ConnectionIO[Int] = {
//      updaters.map(_.execute).sequence.map(_.sum)
//    }
//  }
//
//  class UpdaterReturningKeys[T, K, U, V <: EntityFilter[V]](modifier: U, filter: V) {
//    def fragment: Fragment = ???
//
//    def execute: Stream[ConnectionIO, K] = ???
//  }
//
//  object UpdaterReturningKeys {
//    def multi[T, K, U, V <: EntityFilter[V]](
//        updaters: List[UpdateReturningKeys[T, K, U, V]]
//    ): ConnectionIO[List[K]] = {
//      val a = updaters.map(_.execute)
//      val c = updaters.map(_.execute.compile.toList).sequence.map(_.flatten)
//      c
//    }
//  }
//
//  class Deleter
//
//  class DeleteByKey[T](table: Table[T])
//
//  class DeleterReturningKeys
//}
