package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.table.TableLink.Aux
import shapeless.tag.@@
import shapeless.{HList, LUBConstraint, LabelledGeneric, Lazy, Poly1, SingletonProductArgs, ops, tag}

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

trait FieldSelection[ReprA <: HList, ReprK <: HList] {
  type Values <: HList

  def fields(kRepr: ReprK): List[String]
}

object FieldSelection {

  type Aux[ReprA <: HList, ReprK <: HList, Values0 <: HList] = FieldSelection[ReprA, ReprK] {
    type Values = Values0
  }

  implicit def derive[ReprA <: HList, ReprK <: HList, SelectedAK <: HList](
      implicit
      selectAllAK: ops.record.SelectAll.Aux[ReprA, ReprK, SelectedAK],
      toListK: ops.hlist.ToList[ReprK, Symbol]
  ): Aux[ReprA, ReprK, SelectedAK] = new FieldSelection[ReprA, ReprK] {
    override type Values = SelectedAK

    override def fields(kRepr: ReprK): List[String] = kRepr.toList.map(_.name)
  }
}
