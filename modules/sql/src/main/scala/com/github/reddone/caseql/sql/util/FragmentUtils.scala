package com.github.reddone.caseql.sql.util

import cats.implicits._
import com.github.reddone.caseql.sql.tokens.Not
import doobie._
import Fragment._

object FragmentUtils {

  def wrapInQuery[W: Write, R: Read](fragment: Fragment): Query[W, R] = {
    val query0 = fragment.query[R]
    Query[W, R](query0.sql, query0.pos)
  }

  def wrapInUpdate[W: Write](fragment: Fragment): Update[W] = {
    val update0 = fragment.update
    Update[W](update0.sql, update0.pos)
  }

  def optionalAndOpt(fs: Option[Fragment]*): Option[Fragment] = {
    val united = fs.toList.unite
    if (united.isEmpty) None else Some(Fragments.and(united: _*))
  }

  def optionalOrOpt(fs: Option[Fragment]*): Option[Fragment] = {
    val united = fs.toList.unite
    if (united.isEmpty) None else Some(Fragments.or(united: _*))
  }

  def optionalNot(f: Option[Fragment]): Option[Fragment] = {
    f.map(const(Not) ++ Fragments.parentheses(_))
  }
}
