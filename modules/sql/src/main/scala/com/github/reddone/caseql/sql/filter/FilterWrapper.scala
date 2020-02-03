package com.github.reddone.caseql.sql.filter

import com.github.reddone.caseql.sql.generic.{Table, TableFilter}
import com.github.reddone.caseql.sql.util.FragmentUtils
import doobie._
import doobie.implicits._
import Fragment._

trait FilterWrapper[T <: FilterWrapper[T]] { self: T with Product =>
  def AND: Option[Seq[T]]
  def OR: Option[Seq[T]]
  def NOT: Option[T]
}

object FilterWrapper {

  def filterFragment[T, U <: FilterWrapper[U]](filter: U)(
      implicit
      table: Table[T],
      tableFilter: TableFilter[T, U]
  ): Option[Fragment] = {
    def make(filter: U): Option[Fragment] = {
      val left  = tableFilter.keys().map(_.name).map(table.defaultSyntax.column)
      val right = tableFilter.values(filter)
      val zipped = left.zip(right).map {
        case (col, optionFilter) => optionFilter.flatMap(_.toOptionFragment(col))
      }
      FragmentUtils.optionalAndOpt(zipped: _*)
    }

    FragmentUtils.optionalAndOpt(
      make(filter),
      filter.AND.flatMap { and =>
        val recs = and.map(filterFragment(_))
        FragmentUtils.optionalAndOpt(recs: _*)
      },
      filter.OR.flatMap { or =>
        val recs = or.map(filterFragment(_))
        FragmentUtils.optionalOrOpt(recs: _*)
      },
      filter.NOT.flatMap { not =>
        val rec = filterFragment(not)
        FragmentUtils.optionalNot(rec)
      }
    )
  }
}
