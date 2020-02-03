package com.github.reddone.caseql.sql.generic

import java.io.FilterWriter

import com.github.reddone.caseql.sql.filter.FilterWrapper
import com.github.reddone.caseql.sql.filter.models.{Filter, IntFilter, LongFilter, StringFilter}
import com.github.reddone.caseql.sql.generic.TableFunction.extractFilter
import com.github.reddone.caseql.sql.modifier.models.Modifier
import doobie._
import doobie.implicits._
import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Poly1, Witness, ops}

// We have TableFilter[A, AA] and TableFilter[B, BB]

// 1) we can put RelationFilters inside filters and go unsafe
// 2) There is not way to keep track of relation between A and B inside filter

// a relation between A and B

trait Relation[A, B] {}

object Relation {}

// a filter using relations

trait RelationFilter[A, B, T <: FilterWrapper[T]] {
  def toOptionFragment: Option[Fragment]
}

final case class EveryFilter[A, B, T <: FilterWrapper[T]](filter: T) extends RelationFilter[A, B, T] {
  override def toOptionFragment: Option[Fragment] = None
}

final case class SomeFilter[A, B, T <: FilterWrapper[T]](filter: T) extends RelationFilter[A, B, T] {
  override def toOptionFragment: Option[Fragment] = None
}

final case class NoneFilter[A, B, T <: FilterWrapper[T]](filter: T) extends RelationFilter[A, B, T] {
  override def toOptionFragment: Option[Fragment] = None
}

// 1) extract all RelationFilter
// 2) for each Filter, check that it exists a relation using a typeclass

trait HasRelation[R <: HList] {}

object HasRelation extends lowPriorityHasRelation {

  implicit def hconsHasRelation[T <: FilterWrapper[T], A, B, K, V <: Option[RelationFilter[A, B, T]], R <: HList](
      implicit
      witness: Witness.Aux[K],
      relation: Relation[A, B],
      tableFilter: TableFilter[B, T],
      hasRelation: HasRelation[R]
  ): HasRelation[FieldType[K, V] :: R] = new HasRelation[FieldType[K, V] :: R] {
    //FilterWrapper.filterFragment()
  }
}

trait lowPriorityHasRelation {
  implicit def hnilHasRelation: HasRelation[HNil] = new HasRelation[HNil] {}
}

object extractRelationFilter extends TableFunction.extract[Option[RelationFilter[_, _, _]]]

// test

object Test extends App {

  case class A(field1: String, field2: Int)
  case class B(field1: Long, field2: String)

  implicit val relAB: Relation[A, B] = new Relation[A, B] {}

  case class AFilter(
      field1: Option[StringFilter],
      field2: Option[IntFilter],
      everyB: Option[EveryFilter[A, B, BFilter]],
      someB: Option[SomeFilter[A, B, BFilter]]
  ) extends FilterWrapper[AFilter] {
    override def AND: Option[Seq[AFilter]] = None
    override def OR: Option[Seq[AFilter]]  = None
    override def NOT: Option[AFilter]      = None
  }
  case class BFilter(
      field1: Option[LongFilter],
      field2: Option[StringFilter]
  ) extends FilterWrapper[BFilter] {
    override def AND: Option[Seq[BFilter]] = None
    override def OR: Option[Seq[BFilter]]  = None
    override def NOT: Option[BFilter]      = None
  }

  implicit val filter2: TableFilter[B, BFilter]   = TableFilter.derive[B, BFilter]()
  implicit val filter: TableFilterRel[A, AFilter] = TableFilterRel.derive[A, AFilter]()

  val bFilter = BFilter(
    Some(LongFilter.empty),
    Some(StringFilter.empty)
  )
  val aFilter = AFilter(
    None,
    None,
    Some(EveryFilter[A, B, BFilter](bFilter)),
    None
  )
  println(
    LabelledGeneric[AFilter]
      .to(aFilter)
      .flatMap(extractRelationFilter)
      .toList[Option[RelationFilter[_, _, _]]]
  )
}

// alternative

trait TableFilterRel[T, U] {
  def keys(): List[Symbol]
  def values(u: U): List[Option[Filter[_]]]
}

object TableFilterRel {

  def apply[T, U](implicit ev: TableFilterRel[T, U]): TableFilterRel[T, U] = ev

  object derive {

    def apply[T, U] = new Partial[T, U]

    class Partial[T, U] {

      def apply[L <: HList, R <: HList, RKeys <: HList, RValues <: HList]()(
          implicit
          lgenT: LabelledGeneric.Aux[T, L],
          lgenU: LabelledGeneric.Aux[U, R],
          tableFilterLR: ReprTableFilterRel.Aux[T, L, R, RKeys, RValues],
          keyToTraversableR: ops.hlist.ToTraversable.Aux[RKeys, List, Symbol],
          valueToTraversableR: ops.hlist.ToTraversable.Aux[RValues, List, Option[Filter[_]]]
      ): TableFilterRel[T, U] = new TableFilterRel[T, U] {
        override def keys(): List[Symbol]                  = tableFilterLR.keys().toList
        override def values(u: U): List[Option[Filter[_]]] = tableFilterLR.values(lgenU.to(u)).toList
      }
    }
  }
}

trait ReprTableFilterRel[T, L <: HList, R <: HList] {
  type Keys <: HList
  type Values <: HList

  def keys(): Keys
  def values(r: R): Values
}

object ReprTableFilterRel {
  type OptionFilter[A] = Option[Filter[A]]

  type Aux[T, L <: HList, R <: HList, Keys0 <: HList, Values0 <: HList] = ReprTableFilterRel[T, L, R] {
    type Keys   = Keys0
    type Values = Values0
  }

  implicit def tableFilter[
      T,
      L <: HList,
      LKeys <: HList,
      LValues <: HList,
      LValuesWrapped <: HList,
      LZipped <: HList,
      R <: HList,
      RFilter <: HList,
      RFilterKeys <: HList,
      RFilterValues <: HList,
      RAligned <: HList,
      RRelFilter <: HList
  ](
      implicit
      keysL: ops.record.Keys.Aux[L, LKeys],
      valuesL: ops.record.Values.Aux[L, LValues],
      mappedValuesL: ops.hlist.Mapped.Aux[LValues, OptionFilter, LValuesWrapped],
      zippedL: ops.hlist.ZipWithKeys.Aux[LKeys, LValuesWrapped, LZipped],
      extractFilterR: ops.hlist.FlatMapper.Aux[extractFilter.type, R, RFilter],
      unzippedR: ops.record.UnzipFields.Aux[RFilter, RFilterKeys, RFilterValues],
      alignR: ops.record.AlignByKeys.Aux[RFilter, LKeys, RAligned],
      extTR: <:<[RAligned, LZipped],
      extractRelationTR: ops.hlist.FlatMapper.Aux[extractRelationFilter.type, R, RRelFilter],
      hasRelations: HasRelation[RRelFilter]
  ): Aux[T, L, R, RFilterKeys, RFilterValues] = new ReprTableFilterRel[T, L, R] {
    override type Keys   = RFilterKeys
    override type Values = RFilterValues

    override def keys(): RFilterKeys         = unzippedR.keys()
    override def values(r: R): RFilterValues = unzippedR.values(r.flatMap(extractFilter))
  }
}
