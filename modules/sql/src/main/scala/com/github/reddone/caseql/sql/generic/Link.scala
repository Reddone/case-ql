package com.github.reddone.caseql.sql.generic

import cats.data.NonEmptyList

// TODO: use type safe columns to select fields. It is expensive but it ensures
// TODO: that both tables can be linked together

trait Link[A, B] {
  type C

  def tableA: Table[A]
  def tableB: Table[B]
  def tableC: Table[C]
  def leftJoinFields: List[(String, String)]
  def rightJoinFields: List[(String, String)]
  def isJunction: Boolean
}

object Link {

  type Aux[A, B, C0] = Link[A, B] { type C = C0 }

  def self[A](
      condition: (Table[A]) => NonEmptyList[String]
  )(
      implicit
      tableAA: Table[A]
  ): Aux[A, A, Unit] = new Link[A, A] {
    override type C = Unit
    override def tableA: Table[A]                        = tableAA
    override def tableB: Table[A]                        = tableAA
    override def tableC: Table[C]                        = Table.unit
    override def isJunction: Boolean                     = false
    override def leftJoinFields: List[(String, String)]  = condition(tableAA).toList.zip(condition(tableAA).toList)
    override def rightJoinFields: List[(String, String)] = List.empty
  }

  def direct[A, B](
      condition: (Table[A], Table[B]) => NonEmptyList[(String, String)]
  )(
      implicit
      tableAA: Table[A],
      tableBB: Table[B]
  ): Aux[A, B, Unit] = new Link[A, B] {
    override type C = Unit
    override def tableA: Table[A]                        = tableAA
    override def tableB: Table[B]                        = tableBB
    override def tableC: Table[C]                        = Table.unit
    override def isJunction: Boolean                     = false
    override def leftJoinFields: List[(String, String)]  = condition(tableAA, tableBB).toList
    override def rightJoinFields: List[(String, String)] = List.empty
  }

  def junction[A, B, C0](
      leftCondition: (Table[A], Table[C0]) => NonEmptyList[(String, String)],
      rightCondition: (Table[B], Table[C0]) => NonEmptyList[(String, String)]
  )(
      implicit
      tableAA: Table[A],
      tableBB: Table[B],
      tableCC: Table[C0]
  ): Aux[A, B, C0] = new Link[A, B] {
    override type C = C0
    override def tableA: Table[A]                        = tableAA
    override def tableB: Table[B]                        = tableBB
    override def tableC: Table[C]                        = tableCC
    override def leftJoinFields: List[(String, String)]  = leftCondition(tableAA, tableCC).toList
    override def rightJoinFields: List[(String, String)] = rightCondition(tableBB, tableCC).toList
    override def isJunction: Boolean                     = true
  }

  implicit def inverse[A, B, C0](
      implicit link: Link.Aux[A, B, C0]
  ): Aux[B, A, C0] = new Link[B, A] {
    override type C = C0
    override def tableA: Table[B]                        = link.tableB
    override def tableB: Table[A]                        = link.tableA
    override def tableC: Table[C]                        = link.tableC
    override def leftJoinFields: List[(String, String)]  = link.rightJoinFields
    override def rightJoinFields: List[(String, String)] = link.leftJoinFields
    override def isJunction: Boolean                     = link.isJunction
  }
}
