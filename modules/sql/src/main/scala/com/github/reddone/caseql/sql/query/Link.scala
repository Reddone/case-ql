package com.github.reddone.caseql.sql.query

import cats.data.NonEmptyList

// TODO: use type safe columns to select fields. It is expensive but it ensures
// TODO: that both tables can be linked together

trait Link[A, B] {
  type Junction

  def leftSyntax: Syntax[A]
  def rightSyntax: Syntax[B]
  def junctionSyntax: Syntax[Junction]
  def leftJoinFields: List[(String, String)]
  def rightJoinFields: List[(String, String)]
  def isJunction: Boolean
}

object Link {

  type Aux[A, B, C] = Link[A, B] { type Junction = C }

  def self[A, K](left: Table[A, K])(condition: Syntax[A] => NonEmptyList[String]): Aux[A, A, Unit] = new Link[A, A] {
    override type Junction = Unit

    override def leftSyntax: Syntax[A]                   = left.defaultSyntax
    override def rightSyntax: Syntax[A]                  = left.defaultSyntax
    override def junctionSyntax: Syntax[Junction]        = Table.unit.defaultSyntax
    override def leftJoinFields: List[(String, String)]  = condition(leftSyntax).toList.zip(condition(leftSyntax).toList)
    override def rightJoinFields: List[(String, String)] = List.empty
    override def isJunction: Boolean                     = false
  }

  def direct[A, K, B, J](left: Table[A, K], right: Table[B, J])(
      condition: (Syntax[A], Syntax[B]) => NonEmptyList[(String, String)]
  ): Aux[A, B, Unit] = new Link[A, B] {
    override type Junction = Unit

    override def leftSyntax: Syntax[A]                   = left.defaultSyntax
    override def rightSyntax: Syntax[B]                  = right.defaultSyntax
    override def junctionSyntax: Syntax[Junction]        = Table.unit.defaultSyntax
    override def leftJoinFields: List[(String, String)]  = condition(leftSyntax, rightSyntax).toList
    override def rightJoinFields: List[(String, String)] = List.empty
    override def isJunction: Boolean                     = false
  }

  def junction[A, K, B, J, C, I](left: Table[A, K], right: Table[B, J], junction: Table[C, I])(
      leftCondition: (Syntax[A], Syntax[C]) => NonEmptyList[(String, String)],
      rightCondition: (Syntax[B], Syntax[C]) => NonEmptyList[(String, String)]
  ): Aux[A, B, C] = new Link[A, B] {
    override type Junction = C

    override def leftSyntax: Syntax[A]                   = left.defaultSyntax
    override def rightSyntax: Syntax[B]                  = right.defaultSyntax
    override def junctionSyntax: Syntax[Junction]        = junction.defaultSyntax
    override def leftJoinFields: List[(String, String)]  = leftCondition(leftSyntax, junctionSyntax).toList
    override def rightJoinFields: List[(String, String)] = rightCondition(rightSyntax, junctionSyntax).toList
    override def isJunction: Boolean                     = true
  }

  implicit def inverse[A, B, C](
      implicit link: Link.Aux[A, B, C]
  ): Aux[B, A, C] = new Link[B, A] {
    override type Junction = C
    override def leftSyntax: Syntax[B]                   = link.rightSyntax
    override def rightSyntax: Syntax[A]                  = link.leftSyntax
    override def junctionSyntax: Syntax[Junction]        = link.junctionSyntax
    override def leftJoinFields: List[(String, String)]  = link.rightJoinFields
    override def rightJoinFields: List[(String, String)] = link.leftJoinFields
    override def isJunction: Boolean                     = link.isJunction
  }
}
