package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.table.TableLink.Aux
import shapeless.{HList, LabelledGeneric, Lazy, ops}

trait TableLink[A, B] { self =>
  type Junction

  def leftSyntax: TableSyntax[A]
  def rightSyntax: TableSyntax[B]
  def junctionSyntax: TableSyntax[Junction]
  def leftJoinFields: List[(String, String)]
  def rightJoinFields: List[(String, String)]
  def isJunction: Boolean

  final def inverse: Aux[B, A, Junction] = new TableLink[B, A] {
    override type Junction = self.Junction

    override def leftSyntax: TableSyntax[B]              = self.rightSyntax
    override def rightSyntax: TableSyntax[A]             = self.leftSyntax
    override def junctionSyntax: TableSyntax[Junction]   = self.junctionSyntax
    override def leftJoinFields: List[(String, String)]  = self.rightJoinFields
    override def rightJoinFields: List[(String, String)] = self.leftJoinFields
    override def isJunction: Boolean                     = self.isJunction
  }
}

object TableLink extends LowPriorityTableLink with LowestPriorityTableLink {

  type Aux[A, B, C] = TableLink[A, B] { type Junction = C }

  def apply[A, B](implicit ev: TableLink[A, B]): Aux[A, B, ev.Junction] = ev

  object self {

    def apply[A] = new Partial[A]

    class Partial[A] {

      def apply[
          ReprA <: HList,
          ReprK <: HList,
          ReprJ <: HList,
          ValuesK <: HList,
          ValuesJ <: HList
      ](
          fsa: ReprK,
          fsb: ReprJ
      )(
          implicit
          tableSyntaxA: TableSyntax[A],
          lgenA: LabelledGeneric.Aux[A, ReprA],
          fieldSelectionAK: Lazy[FieldSelection.Aux[ReprA, ReprK, ValuesK]],
          fieldSelectionAJ: Lazy[FieldSelection.Aux[ReprA, ReprJ, ValuesJ]],
          sameValuesKJ: ValuesK =:= ValuesJ
      ): Aux[A, A, Unit] = new TableLink[A, A] {
        override type Junction = Unit

        override def leftSyntax: TableSyntax[A]            = tableSyntaxA
        override def rightSyntax: TableSyntax[A]           = tableSyntaxA
        override def junctionSyntax: TableSyntax[Junction] = Table.unit.syntax
        override def leftJoinFields: List[(String, String)] =
          fieldSelectionAK.value.fields(fsa).zip(fieldSelectionAJ.value.fields(fsb))
        override def rightJoinFields: List[(String, String)] = leftJoinFields.map(_.swap)
        override def isJunction: Boolean                     = false
      }
    }
  }

  object direct {

    def apply[A, B] = new Partial[A, B]

    class Partial[A, B] {

      def apply[
          ReprA <: HList,
          ReprB <: HList,
          ReprK <: HList,
          ReprJ <: HList,
          ValuesK <: HList,
          ValuesJ <: HList
      ](
          fsa: ReprK,
          fsb: ReprJ
      )(
          implicit
          tableSyntaxA: TableSyntax[A],
          tableSyntaxB: TableSyntax[B],
          lgenA: LabelledGeneric.Aux[A, ReprA],
          lgenB: LabelledGeneric.Aux[B, ReprB],
          fieldSelectionAK: Lazy[FieldSelection.Aux[ReprA, ReprK, ValuesK]],
          fieldSelectionBJ: Lazy[FieldSelection.Aux[ReprB, ReprJ, ValuesJ]],
          sameValuesKJ: ValuesK =:= ValuesJ
      ): Aux[A, B, Unit] = new TableLink[A, B] {
        override type Junction = Unit

        override def leftSyntax: TableSyntax[A]            = tableSyntaxA
        override def rightSyntax: TableSyntax[B]           = tableSyntaxB
        override def junctionSyntax: TableSyntax[Junction] = Table.unit.syntax
        override def leftJoinFields: List[(String, String)] =
          fieldSelectionAK.value.fields(fsa).zip(fieldSelectionBJ.value.fields(fsb))
        override def rightJoinFields: List[(String, String)] = leftJoinFields.map(_.swap)
        override def isJunction: Boolean                     = false
      }
    }
  }

  implicit def junction[A, B, C](
      implicit
      leftLink: Aux[A, C, Unit],
      rightLink: Aux[B, C, Unit]
  ): Aux[A, B, C] = new TableLink[A, B] {
    override type Junction = C

    override def leftSyntax: TableSyntax[A]              = leftLink.leftSyntax
    override def rightSyntax: TableSyntax[B]             = rightLink.leftSyntax
    override def junctionSyntax: TableSyntax[Junction]   = rightLink.rightSyntax
    override def leftJoinFields: List[(String, String)]  = leftLink.leftJoinFields
    override def rightJoinFields: List[(String, String)] = rightLink.leftJoinFields
    override def isJunction: Boolean                     = true
  }
}

trait LowPriorityTableLink {

  implicit def junctionLeftInverse[A, B, C](
      implicit
      leftLink: Aux[C, A, Unit],
      rightLink: Aux[B, C, Unit]
  ): Aux[A, B, C] = TableLink.junction(leftLink.inverse, rightLink)
}

trait LowestPriorityTableLink {

  implicit def junctionRightInverse[A, B, C](
      implicit
      leftLink: Aux[A, C, Unit],
      rightLink: Aux[C, B, Unit]
  ): Aux[A, B, C] = TableLink.junction(leftLink, rightLink.inverse)
}
