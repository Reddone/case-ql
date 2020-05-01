package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.modifier.primitives.Modifier
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import doobie._
import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness, ops}

trait TableModifier[A, MA <: EntityModifier[MA]] {
  def primitiveModifierNamedFragments(modifier: MA): List[(String, Option[Fragment])]
}

object TableModifier {

  def apply[A, MA <: EntityModifier[MA]](implicit ev: TableModifier[A, MA]): TableModifier[A, MA] = ev

  object derive {

    def apply[A, MA <: EntityModifier[MA]] = new Partial[A, MA]

    class Partial[A, MA <: EntityModifier[MA]] {

      def apply[ReprA <: HList, ReprMA <: HList]()(
          implicit
          lgenA: LabelledGeneric.Aux[A, ReprA],
          lgenMA: LabelledGeneric.Aux[MA, ReprMA],
          tableModifierMA: Lazy[ReprTableModifier[A, ReprA, ReprMA]]
      ): TableModifier[A, MA] = new TableModifier[A, MA] {
        override def primitiveModifierNamedFragments(modifier: MA): List[(String, Option[Fragment])] = {
          tableModifierMA.value.primitiveModifierNamedFragments(lgenMA.to(modifier)).reverse
        }
      }
    }
  }
}

trait ReprTableModifier[A, ReprA <: HList, ReprMA] {
  def primitiveModifierNamedFragments(modifierRepr: ReprMA): List[(String, Option[Fragment])]
}

object ReprTableModifier extends LowPriorityReprTableModifier {

  implicit def hconsTableModifier[A, ReprA <: HList, K <: Symbol, V, T <: HList](
      implicit
      witness: Witness.Aux[K],
      headModifier: ReprTableModifier[A, ReprA, FieldType[K, V]],
      tailModifier: ReprTableModifier[A, ReprA, T]
  ): ReprTableModifier[A, ReprA, FieldType[K, V] :: T] =
    new ReprTableModifier[A, ReprA, FieldType[K, V] :: T] {
      override def primitiveModifierNamedFragments(
          modifierRepr: FieldType[K, V] :: T
      ): List[(String, Option[Fragment])] = {
        tailModifier.primitiveModifierNamedFragments(modifierRepr.tail) ++
          headModifier.primitiveModifierNamedFragments(modifierRepr.head)
      }
    }
}

trait LowPriorityReprTableModifier {

  implicit def primitiveTableModifier[A, ReprA <: HList, K <: Symbol, V, U, T](
      implicit
      witness: Witness.Aux[K],
      keySelector: ops.record.Selector.Aux[ReprA, K, U],
      modifierHasSameType: U =:= T,
      valueEvidence: V <:< Option[Modifier[T]],
      tableSyntax: TableSyntax[A]
  ): ReprTableModifier[A, ReprA, FieldType[K, V]] =
    new ReprTableModifier[A, ReprA, FieldType[K, V]] {
      override def primitiveModifierNamedFragments(
          modifierRepr: FieldType[K, V]
      ): List[(String, Option[Fragment])] = {
        val column = tableSyntax.column(witness.value.name)
        List(modifierRepr.map(m => (column, Some(m.processPrimitiveModifier))).getOrElse((column, None)))
      }
    }

  implicit def hnilTableModifier[A, ReprA <: HList]: ReprTableModifier[A, ReprA, HNil] =
    new ReprTableModifier[A, ReprA, HNil] {
      override def primitiveModifierNamedFragments(
          modifierRepr: HNil
      ): List[(String, Option[Fragment])] = Nil
    }
}
