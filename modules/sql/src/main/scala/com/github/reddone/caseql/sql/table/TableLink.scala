package com.github.reddone.caseql.sql.table

import cats.data.NonEmptyList
import shapeless.tag.@@
import shapeless.{HList, LUBConstraint, LabelledGeneric, Nat, Poly1, SingletonProductArgs, Witness, ops, tag}

object toTaggedSymbol extends Poly1 {
  implicit def atString[K <: String]: Case.Aux[K, Symbol @@ K] =
    at[K] { k =>
      new tag.Tagger[K].apply(Symbol(k))
    }
}

object FieldSet extends SingletonProductArgs {

  def applyProduct[L <: HList](l: L)(implicit ev: LUBConstraint[L, String]): L = l
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

  object safe {

    def apply[A, B] = new Partial[A, B]

    class Partial[A, B] {

      def apply[
          ReprA <: HList,
          ReprB <: HList,
          ReprK <: HList,
          ReprJ <: HList,
          ReprK2 <: HList,
          ReprJ2 <: HList,
          N1 <: Nat,
          N2 <: Nat
      ](
          a: ReprK,
          b: ReprJ
      )(
          implicit
          lgenA: LabelledGeneric.Aux[A, ReprA],
          lgenB: LabelledGeneric.Aux[B, ReprB],
          leftTableSyntax: TableSyntax[A],
          rightTableSyntax: TableSyntax[B],
          mappedA: ops.hlist.Mapper.Aux[toTaggedSymbol.type, ReprK, ReprK2],
          mappedB: ops.hlist.Mapper.Aux[toTaggedSymbol.type, ReprJ, ReprJ2],
          selA: ops.record.SelectAll[ReprA, ReprK2],
          selB: ops.record.SelectAll[ReprB, ReprJ2],
          toListA: ops.hlist.ToList[ReprK2, Symbol],
          toListB: ops.hlist.ToList[ReprJ2, Symbol],
          lenA: ops.hlist.Length.Aux[ReprK, N1],
          lenB: ops.hlist.Length.Aux[ReprJ, N2],
          ev: N1 =:= N2
      ): Aux[A, B, Unit] = new TableLink[A, B] {
        override type Junction = Unit

        override def leftSyntax: TableSyntax[A] = leftTableSyntax

        override def rightSyntax: TableSyntax[B] = rightTableSyntax

        override def junctionSyntax: TableSyntax[Junction] = Table.unit.syntax

        override def leftJoinFields: List[(String, String)] =
          a.map(toTaggedSymbol).toList.map(_.name).zip(b.map(toTaggedSymbol).toList.map(_.name))

        override def rightJoinFields: List[(String, String)] = List.empty

        override def isJunction: Boolean = false
      }
    }
  }

  def self[A, K](left: Table[A, K])(
      condition: TableSyntax[A] => NonEmptyList[String]
  ): Aux[A, A, Unit] = new TableLink[A, A] {
    override type Junction = Unit

    override def leftSyntax: TableSyntax[A]            = left.syntax
    override def rightSyntax: TableSyntax[A]           = left.syntax
    override def junctionSyntax: TableSyntax[Junction] = Table.unit.syntax
    override def leftJoinFields: List[(String, String)] =
      condition(leftSyntax).toList.zip(condition(leftSyntax).toList)
    override def rightJoinFields: List[(String, String)] = List.empty
    override def isJunction: Boolean                     = false
  }

  def direct[A, K, B, J](left: Table[A, K], right: Table[B, J])(
      condition: (TableSyntax[A], TableSyntax[B]) => NonEmptyList[(String, String)]
  ): Aux[A, B, Unit] = new TableLink[A, B] {
    override type Junction = Unit

    override def leftSyntax: TableSyntax[A]              = left.syntax
    override def rightSyntax: TableSyntax[B]             = right.syntax
    override def junctionSyntax: TableSyntax[Junction]   = Table.unit.syntax
    override def leftJoinFields: List[(String, String)]  = condition(leftSyntax, rightSyntax).toList
    override def rightJoinFields: List[(String, String)] = List.empty
    override def isJunction: Boolean                     = false
  }

  def junction[A, K, B, J, C, I](left: Table[A, K], right: Table[B, J], junction: Table[C, I])(
      leftCondition: (TableSyntax[A], TableSyntax[C]) => NonEmptyList[(String, String)],
      rightCondition: (TableSyntax[B], TableSyntax[C]) => NonEmptyList[(String, String)]
  ): Aux[A, B, C] = new TableLink[A, B] {
    override type Junction = C

    override def leftSyntax: TableSyntax[A]              = left.syntax
    override def rightSyntax: TableSyntax[B]             = right.syntax
    override def junctionSyntax: TableSyntax[Junction]   = junction.syntax
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
