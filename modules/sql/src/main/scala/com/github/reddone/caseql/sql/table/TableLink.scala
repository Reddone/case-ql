package com.github.reddone.caseql.sql.table

import shapeless.tag.@@
import shapeless.{HList, LUBConstraint, LabelledGeneric, Lazy, Nat, Poly1, SingletonProductArgs, ops, tag}

object toTaggedSymbol extends Poly1 {
  implicit def atString[K <: String]: Case.Aux[K, Symbol @@ K] =
    at[K] { k =>
      new tag.Tagger[K].apply(Symbol(k))
    }
}

object FieldSet extends SingletonProductArgs {

  def applyProduct[L <: HList, MappedL <: HList](l: L)(
      implicit
      lubL: LUBConstraint[L, String],
      taggedSymbolL: ops.hlist.Mapper.Aux[toTaggedSymbol.type, L, MappedL]
  ): MappedL = l.map(toTaggedSymbol)
}

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

  object self {

    def apply[A] = new Partial[A]

    class Partial[A] {

      def apply[ReprA <: HList, ReprK <: HList, ReprJ <: HList, N <: Nat, M <: Nat](
          fsa: ReprK,
          fsb: ReprJ
      )(
          implicit
          tableSyntaxA: TableSyntax[A],
          lgenA: LabelledGeneric.Aux[A, ReprA],
          fieldSelectionAK: Lazy[FieldSelection.Aux[ReprA, ReprK, N]],
          fieldSelectionAJ: Lazy[FieldSelection.Aux[ReprA, ReprJ, M]],
          ev: N =:= M
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

      def apply[ReprA <: HList, ReprB <: HList, ReprK <: HList, ReprJ <: HList, N <: Nat, M <: Nat](
          fsa: ReprK,
          fsb: ReprJ
      )(
          implicit
          tableSyntaxA: TableSyntax[A],
          tableSyntaxB: TableSyntax[B],
          lgenA: LabelledGeneric.Aux[A, ReprA],
          lgenB: LabelledGeneric.Aux[B, ReprB],
          fieldSelectionAK: Lazy[FieldSelection.Aux[ReprA, ReprK, N]],
          fieldSelectionBJ: Lazy[FieldSelection.Aux[ReprB, ReprJ, M]],
          ev: N =:= M
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

  implicit def inverse[A, B](
      implicit link: Aux[A, B, Unit]
  ): Aux[B, A, link.Junction] = new TableLink[B, A] {
    override type Junction = link.Junction

    override def leftSyntax: TableSyntax[B]              = link.rightSyntax
    override def rightSyntax: TableSyntax[A]             = link.leftSyntax
    override def junctionSyntax: TableSyntax[Junction]   = link.junctionSyntax
    override def leftJoinFields: List[(String, String)]  = link.rightJoinFields
    override def rightJoinFields: List[(String, String)] = link.leftJoinFields
    override def isJunction: Boolean                     = link.isJunction
  }

  implicit def junction[A, B, C](
      implicit
      leftLink: Lazy[Aux[A, C, Unit]],
      rightLink: Lazy[Aux[B, C, Unit]]
  ): Aux[A, B, C] = new TableLink[A, B] {
    override type Junction = C

    override def leftSyntax: TableSyntax[A]              = leftLink.value.leftSyntax
    override def rightSyntax: TableSyntax[B]             = rightLink.value.leftSyntax
    override def junctionSyntax: TableSyntax[Junction]   = rightLink.value.rightSyntax
    override def leftJoinFields: List[(String, String)]  = leftLink.value.leftJoinFields
    override def rightJoinFields: List[(String, String)] = rightLink.value.rightJoinFields
    override def isJunction: Boolean                     = true
  }
}

trait FieldSelection[ReprA <: HList, ReprK <: HList] {
  type Len <: Nat

  def fields(kRepr: ReprK): List[String]
}

object FieldSelection {

  type Aux[ReprA <: HList, ReprK <: HList, Len0 <: Nat] = FieldSelection[ReprA, ReprK] { type Len = Len0 }

  implicit def derive[ReprA <: HList, ReprK <: HList, N <: Nat](
      implicit
      selectAllAK: ops.record.SelectAll[ReprA, ReprK],
      toListK: ops.hlist.ToList[ReprK, Symbol],
      lenK: ops.hlist.Length.Aux[ReprK, N]
  ): Aux[ReprA, ReprK, N] =
    new FieldSelection[ReprA, ReprK] {
      override type Len = N

      override def fields(kRepr: ReprK): List[String] = kRepr.toList.map(_.name)
    }
}
