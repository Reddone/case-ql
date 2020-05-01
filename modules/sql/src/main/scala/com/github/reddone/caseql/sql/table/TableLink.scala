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
          tableJoinAKJ: Lazy[TableJoin[ReprA, ReprA, ReprK, ReprJ]]
      ): Aux[A, A, Unit] = new TableLink[A, A] {
        override type Junction = Unit

        override def leftSyntax: TableSyntax[A]              = tableSyntaxA
        override def rightSyntax: TableSyntax[A]             = tableSyntaxA
        override def junctionSyntax: TableSyntax[Junction]   = Table.unit.syntax
        override def leftJoinFields: List[(String, String)]  = tableJoinAKJ.value.joinFields(fsa, fsb)
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
          tableJoinABKJ: Lazy[TableJoin[ReprA, ReprB, ReprK, ReprJ]]
      ): Aux[A, B, Unit] = new TableLink[A, B] {
        override type Junction = Unit

        override def leftSyntax: TableSyntax[A]              = tableSyntaxA
        override def rightSyntax: TableSyntax[B]             = tableSyntaxB
        override def junctionSyntax: TableSyntax[Junction]   = Table.unit.syntax
        override def leftJoinFields: List[(String, String)]  = tableJoinABKJ.value.joinFields(fsa, fsb)
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
          tableJoinACKIL: Lazy[TableJoin[ReprA, ReprC, ReprK, ReprIL]],
          tableJoinBCJIR: Lazy[TableJoin[ReprB, ReprC, ReprJ, ReprIR]]
      ): Aux[A, B, C] = new TableLink[A, B] {
        override type Junction = C

        override def leftSyntax: TableSyntax[A]              = tableSyntaxA
        override def rightSyntax: TableSyntax[B]             = tableSyntaxB
        override def junctionSyntax: TableSyntax[Junction]   = tableSyntaxC
        override def leftJoinFields: List[(String, String)]  = tableJoinACKIL.value.joinFields(fsac._1, fsac._2)
        override def rightJoinFields: List[(String, String)] = tableJoinBCJIR.value.joinFields(fsbc._1, fsbc._2)
        override def isJunction: Boolean                     = true
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

trait TableJoin[ReprA <: HList, ReprB <: HList, ReprK <: HList, ReprJ <: HList] {
  def leftFields(leftRepr: ReprK): List[String]
  def rightFields(rightRepr: ReprJ): List[String]

  final def joinFields(leftRepr: ReprK, rightRepr: ReprJ): List[(String, String)] =
    leftFields(leftRepr).zip(rightFields(rightRepr))
}

object TableJoin {

  implicit def derive[
      ReprA <: HList,
      ReprB <: HList,
      ReprK <: HList,
      ReprJ <: HList,
      ValuesK <: HList,
      ValuesJ <: HList
  ](
      implicit
      selectAllAK: ops.record.SelectAll.Aux[ReprA, ReprK, ValuesK],
      selectAllBJ: ops.record.SelectAll.Aux[ReprB, ReprJ, ValuesJ],
      toListK: ops.hlist.ToList[ReprK, Symbol],
      toListJ: ops.hlist.ToList[ReprJ, Symbol],
      sameValuesKJ: EqualNoOption[ValuesK, ValuesJ]
  ): TableJoin[ReprA, ReprB, ReprK, ReprJ] = new TableJoin[ReprA, ReprB, ReprK, ReprJ] {
    override def leftFields(leftRepr: ReprK): List[String] = leftRepr.toList.map(_.name)

    override def rightFields(rightRepr: ReprJ): List[String] = rightRepr.toList.map(_.name)
  }
}
