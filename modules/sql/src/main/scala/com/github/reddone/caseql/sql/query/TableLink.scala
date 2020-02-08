package com.github.reddone.caseql.sql.query

import cats.data.NonEmptyList

// TODO: use type safe columns to select fields. It is expensive but it ensures
// TODO: that both tables can be linked together

trait TableLink[A, B] {
  type Junction

  def leftSyntax: TableSyntax[A]
  def rightSyntax: TableSyntax[B]
  def junctionSyntax: TableSyntax[Junction]
  def leftJoinFields: List[(String, String)]
  def rightJoinFields: List[(String, String)]
  def isJunction: Boolean
}

object TableLink {

  type Aux[A, B, C] = TableLink[A, B] { type Junction = C }

  def self[A, K](left: Table[A, K])(
      condition: TableSyntax[A] => NonEmptyList[String]
  ): Aux[A, A, Unit] = new TableLink[A, A] {
    override type Junction = Unit

    override def leftSyntax: TableSyntax[A]            = left.internalSyntax
    override def rightSyntax: TableSyntax[A]           = left.internalSyntax
    override def junctionSyntax: TableSyntax[Junction] = Table.unit.internalSyntax
    override def leftJoinFields: List[(String, String)] =
      condition(leftSyntax).toList.zip(condition(leftSyntax).toList)
    override def rightJoinFields: List[(String, String)] = List.empty
    override def isJunction: Boolean                     = false
  }

  def direct[A, K, B, J](left: Table[A, K], right: Table[B, J])(
      condition: (TableSyntax[A], TableSyntax[B]) => NonEmptyList[(String, String)]
  ): Aux[A, B, Unit] = new TableLink[A, B] {
    override type Junction = Unit

    override def leftSyntax: TableSyntax[A]              = left.internalSyntax
    override def rightSyntax: TableSyntax[B]             = right.internalSyntax
    override def junctionSyntax: TableSyntax[Junction]   = Table.unit.internalSyntax
    override def leftJoinFields: List[(String, String)]  = condition(leftSyntax, rightSyntax).toList
    override def rightJoinFields: List[(String, String)] = List.empty
    override def isJunction: Boolean                     = false
  }

  def junction[A, K, B, J, C, I](left: Table[A, K], right: Table[B, J], junction: Table[C, I])(
      leftCondition: (TableSyntax[A], TableSyntax[C]) => NonEmptyList[(String, String)],
      rightCondition: (TableSyntax[B], TableSyntax[C]) => NonEmptyList[(String, String)]
  ): Aux[A, B, C] = new TableLink[A, B] {
    override type Junction = C

    override def leftSyntax: TableSyntax[A]              = left.internalSyntax
    override def rightSyntax: TableSyntax[B]             = right.internalSyntax
    override def junctionSyntax: TableSyntax[Junction]   = junction.internalSyntax
    override def leftJoinFields: List[(String, String)]  = leftCondition(leftSyntax, junctionSyntax).toList
    override def rightJoinFields: List[(String, String)] = rightCondition(rightSyntax, junctionSyntax).toList
    override def isJunction: Boolean                     = true
  }

  implicit def inverse[A, B, C](
      implicit link: TableLink.Aux[A, B, C]
  ): Aux[B, A, C] = new TableLink[B, A] {
    override type Junction = C

    override def leftSyntax: TableSyntax[B]              = link.rightSyntax
    override def rightSyntax: TableSyntax[A]             = link.leftSyntax
    override def junctionSyntax: TableSyntax[Junction]   = link.junctionSyntax
    override def leftJoinFields: List[(String, String)]  = link.rightJoinFields
    override def rightJoinFields: List[(String, String)] = link.leftJoinFields
    override def isJunction: Boolean                     = link.isJunction
  }
}
