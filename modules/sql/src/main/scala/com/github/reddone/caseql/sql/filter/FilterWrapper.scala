package com.github.reddone.caseql.sql.filter

trait FilterWrapper[T <: FilterWrapper[T]] { self: T with Product =>
  def AND: Option[Seq[T]]
  def OR: Option[Seq[T]]
  def NOT: Option[T]
}
