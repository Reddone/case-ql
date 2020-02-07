package com.github.reddone.caseql.sql.generic.ops

import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.generic.{Table, TableFilter}
import com.github.reddone.caseql.sql.tokens.{And, Placeholder}
import com.github.reddone.caseql.sql.util.FragmentUtils
import doobie._
import Fragment._

object FilterOps {

  def byKeyConditionFragment[T, K <: Table[T]#Key](syntax: Table[T]#Syntax, key: K)(
      implicit write: Write[K]
  ): Fragment = {
    FragmentUtils
      .wrapInUpdate[K](const(syntax.keyColumns.map(col => s"$col = $Placeholder").mkString(s" $And ")))
      .toFragment(key)
  }

  def byFilterConditionFragment[T, FT <: EntityFilter[FT]](syntax: Table[T]#Syntax, filter: FT)(
      implicit tableFilter: TableFilter[T, FT]
  ): Option[Fragment] = {
    FragmentUtils.optionalAndOpt(
      FragmentUtils.optionalAndOpt(tableFilter.entityFilterFragments(filter)(syntax): _*),
      FragmentUtils.optionalAndOpt(tableFilter.relationFilterFragments(filter)(syntax): _*),
      filter.AND.flatMap { and =>
        val recs = and.map(byFilterConditionFragment(syntax, _))
        FragmentUtils.optionalAndOpt(recs: _*)
      },
      filter.OR.flatMap { or =>
        val recs = or.map(byFilterConditionFragment(syntax, _))
        FragmentUtils.optionalOrOpt(recs: _*)
      },
      filter.NOT.flatMap { not =>
        val rec = byFilterConditionFragment(syntax, not)
        FragmentUtils.optionalNot(rec)
      }
    )
  }
}
