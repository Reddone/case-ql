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

object TableLink {

  type Aux[A, B, C] = TableLink[A, B] { type Junction = C }

  def apply[A, B](implicit ev: TableLink[A, B]): Aux[A, B, ev.Junction] = ev

  implicit def inverse[A, B](implicit ev: TableLink[A, B]): Aux[B, A, ev.Junction] = ev.inverse

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
          selectAllAK: ops.record.SelectAll.Aux[ReprA, ReprK, ValuesK],
          selectAllAJ: ops.record.SelectAll.Aux[ReprA, ReprJ, ValuesJ],
          toListK: ops.hlist.ToList[ReprK, Symbol],
          toListJ: ops.hlist.ToList[ReprJ, Symbol],
          sameValuesKJ: EqualNoOption[ValuesK, ValuesJ]
      ): Aux[A, A, Unit] = new TableLink[A, A] {
        override type Junction = Unit

        override def leftSyntax: TableSyntax[A]              = tableSyntaxA
        override def rightSyntax: TableSyntax[A]             = tableSyntaxA
        override def junctionSyntax: TableSyntax[Junction]   = Table.unit.syntax
        override def leftJoinFields: List[(String, String)]  = fsa.toList.map(_.name).zip(fsb.toList.map(_.name))
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
          selectAllAK: ops.record.SelectAll.Aux[ReprA, ReprK, ValuesK],
          selectAllBJ: ops.record.SelectAll.Aux[ReprB, ReprJ, ValuesJ],
          toListK: ops.hlist.ToList[ReprK, Symbol],
          toListJ: ops.hlist.ToList[ReprJ, Symbol],
          sameValuesKJ: EqualNoOption[ValuesK, ValuesJ]
      ): Aux[A, B, Unit] = new TableLink[A, B] {
        override type Junction = Unit

        override def leftSyntax: TableSyntax[A]              = tableSyntaxA
        override def rightSyntax: TableSyntax[B]             = tableSyntaxB
        override def junctionSyntax: TableSyntax[Junction]   = Table.unit.syntax
        override def leftJoinFields: List[(String, String)]  = fsa.toList.map(_.name).zip(fsb.toList.map(_.name))
        override def rightJoinFields: List[(String, String)] = leftJoinFields.map(_.swap)
        override def isJunction: Boolean                     = false
      }
    }
  }

  object junction {

    def apply[A, B, C] = new Partial[A, B, C]

    class Partial[A, B, C] {

      def apply[
          ReprA <: HList,
          ReprB <: HList,
          ReprC <: HList,
          ReprK <: HList,
          ReprJ <: HList,
          ReprIL <: HList,
          ReprIR <: HList,
          ValuesK <: HList,
          ValuesJ <: HList,
          ValuesIL <: HList,
          ValuesIR <: HList
      ](
          fsac: (ReprK, ReprIL),
          fsbc: (ReprJ, ReprIR)
      )(
          implicit
          tableSyntaxA: TableSyntax[A],
          tableSyntaxB: TableSyntax[B],
          tableSyntaxC: TableSyntax[C],
          lgenA: LabelledGeneric.Aux[A, ReprA],
          lgenB: LabelledGeneric.Aux[B, ReprB],
          lgenC: LabelledGeneric.Aux[C, ReprC],
          selectAllAK: ops.record.SelectAll.Aux[ReprA, ReprK, ValuesK],
          selectAllCIL: ops.record.SelectAll.Aux[ReprC, ReprIL, ValuesIL],
          selectAllBJ: ops.record.SelectAll.Aux[ReprB, ReprJ, ValuesJ],
          selectAllCIR: ops.record.SelectAll.Aux[ReprC, ReprIR, ValuesIR],
          toListK: ops.hlist.ToList[ReprK, Symbol],
          toListIL: ops.hlist.ToList[ReprIL, Symbol],
          toListJ: ops.hlist.ToList[ReprJ, Symbol],
          toListIR: ops.hlist.ToList[ReprIR, Symbol],
          sameValuesKIL: EqualNoOption[ValuesK, ValuesIL],
          sameValuesJIR: EqualNoOption[ValuesJ, ValuesIR]
      ): Aux[A, B, C] = new TableLink[A, B] {
        override type Junction = C

        override def leftSyntax: TableSyntax[A]            = tableSyntaxA
        override def rightSyntax: TableSyntax[B]           = tableSyntaxB
        override def junctionSyntax: TableSyntax[Junction] = tableSyntaxC
        override def leftJoinFields: List[(String, String)] =
          fsac._1.toList.map(_.name).zip(fsac._2.toList.map(_.name))
        override def rightJoinFields: List[(String, String)] =
          fsbc._1.toList.map(_.name).zip(fsbc._2.toList.map(_.name))
        override def isJunction: Boolean = true
      }
    }
  }

  object union {

    def apply[A, B, C](leftLink: Aux[A, C, Unit], rightLink: Aux[B, C, Unit]): Aux[A, B, C] = new TableLink[A, B] {
      override type Junction = C

      override def leftSyntax: TableSyntax[A]              = leftLink.leftSyntax
      override def rightSyntax: TableSyntax[B]             = rightLink.leftSyntax
      override def junctionSyntax: TableSyntax[Junction]   = rightLink.rightSyntax
      override def leftJoinFields: List[(String, String)]  = leftLink.leftJoinFields
      override def rightJoinFields: List[(String, String)] = rightLink.leftJoinFields
      override def isJunction: Boolean                     = true
    }
  }
}
