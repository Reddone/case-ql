package com.github.reddone.caseql.gql.model

object db {

  final case class QueryResultSet[+T](
      records: List[T],
      count: Long
  )

  object QueryResultSet {
    def empty[T]: QueryResultSet[T] = QueryResultSet[T](Nil, 0L)
  }
}
