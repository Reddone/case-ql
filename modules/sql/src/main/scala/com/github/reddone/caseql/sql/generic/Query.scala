package com.github.reddone.caseql.sql.generic

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import doobie.free.connection.ConnectionIO
import doobie.util.fragment.Fragment
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

sealed trait SelectBuilderState
sealed trait Initialized
sealed trait WithTable
sealed trait WithFilter
sealed trait Completed

class SelectBuilder[S <: SelectBuilderState, T, FT <: EntityFilter[FT]](
    table: Option[Table[T]],
    filter: Option[EntityFilter[FT]]
) {

  def withTable[T0](table: Table[T0])(
      implicit ev: S =:= Initialized
  ): SelectBuilder[S with WithTable, T0, FT] = ???

  def withFilter[FT0](filter: EntityFilter[FT0])(
      implicit
      ev: S =:= Initialized with WithTable,
      tableFilter: TableFilter[T, FT0]
  ): SelectBuilder[S with WithFilter, T, FT0] = ???

  def build(implicit ev: S =:= Initialized with WithTable with WithFilter) = ???
}

object SelectBuilder {

  def apply = new SelectBuilder[Initialized, _, _](None, None)
}

trait SQLAction[R] {
  def toFragment: Fragment
  def execute: ConnectionIO[R]
}

class SelectWhere[T, FT](table: Table[T], filter: EntityFilter[FT]) {
  def toFragment: Fragment
}

trait Mutation

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
