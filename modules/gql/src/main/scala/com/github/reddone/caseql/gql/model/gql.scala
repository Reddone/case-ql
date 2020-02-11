package com.github.reddone.caseql.gql.model

import db.QueryResultSet

object gql {

  trait Identifiable {
    type Identity

    def id: Identity
  }

  trait IntIdentifiable extends Identifiable {
    type Identity = Int
  }

  trait LongIdentifiable extends Identifiable {
    type Identity = Long
  }

  trait StringIdentifiable extends Identifiable {
    type Identity = String
  }

  final case class ListContainer[+A](
      content: Seq[A],
      pageInfo: PageInfo
  )

  object ListContainer {

    def of[A, B](queryResultSet: QueryResultSet[B], offset: Int, limit: Int)(
        recordsMapper: Seq[B] => Seq[A]
    ): ListContainer[A] = {
      ListContainer(
        recordsMapper(queryResultSet.records),
        PageInfo.of(offset, limit, queryResultSet.count.toInt)
      )
    }

    def of[A, B](queryResultSet: QueryResultSet[B], offset: Option[Int], limit: Option[Int])(
        recordsMapper: Seq[B] => Seq[A]
    ): ListContainer[A] = {
      ListContainer(
        recordsMapper(queryResultSet.records),
        PageInfo.of(offset, limit, queryResultSet.count.toInt)
      )
    }

    def empty[A]: ListContainer[A] = ListContainer(Seq.empty[A], PageInfo.empty)
  }

  final case class PageInfo(hasPrevious: Boolean, hasNext: Boolean, total: Int)

  object PageInfo {

    def of(offset: Int, limit: Int, total: Int): PageInfo = {
      val hasPrevious = offset > 0
      val hasNext     = offset + limit < total

      PageInfo(hasPrevious, hasNext, total)
    }

    def of(offset: Option[Int], limit: Option[Int], total: Int): PageInfo = {
      val info = (offset, limit) match {
        case (None, None)           => (false, false)
        case (Some(off), None)      => (off > 0, false)
        case (None, Some(lim))      => (false, lim < total)
        case (Some(off), Some(lim)) => (off > 0, off + lim < total)
      }

      PageInfo(info._1, info._2, total)
    }

    val empty: PageInfo = PageInfo(hasPrevious = false, hasNext = false, 0)
  }
}
