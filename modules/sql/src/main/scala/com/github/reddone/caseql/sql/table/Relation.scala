package com.github.reddone.caseql.sql.table

import cats.data.NonEmptyList
import com.github.reddone.caseql.sql.filter.models.{IntFilter, LongFilter, StringFilter}
import com.github.reddone.caseql.sql.filter.wrappers.{EntityFilter, RelationFilter}
import com.github.reddone.caseql.sql.table.TableFunction.{extractRelationFilter, relationFilterToOptionFragment}
import doobie._
import shapeless.LabelledGeneric

// experiment for replacing filter wrapper
//final case class EntityFilter[T](
//    t: Option[T],
//    AND: Option[Seq[EntityFilter[T]]],
//    OR: Option[Seq[EntityFilter[T]]],
//    NOT: Option[EntityFilter[T]]
//)
// We have to add filter keyword to every object
// {
//   filter: {
//    a: { EQ: 3, GT: 5}
//    b: { EQ: 2},
//    relation: {
//      EVERY: {
//        filter: {}
//      }
//    }
//   },
//   AND: [
//     AND: [{filter: {}, AND: []}, filter: {}],
//     OR: []
//   ]
//
//
// }

object Relation extends App {

  case class A(field1: String, field2: Int)
  case class AKey(field1: String)
  case class B(field1: Long, field2: String)
  case class BKey(field1: Long)

  implicit val tableA: Table[A, AKey] = Table.derive[A, AKey]()
  implicit val tableB: Table[B, BKey] = Table.derive[B, BKey]()

  implicit val relAB: TableLink[A, B] = TableLink.direct(tableA, tableB) { (a, b) =>
    NonEmptyList.of(("field1", "field2"))
  }

  case class AFilter(
      field1: Option[StringFilter],
      field2: Option[IntFilter],
      bRelation: Option[RelationFilter[A, B, BFilter]]
  ) extends EntityFilter[AFilter] {
    override def AND: Option[Seq[AFilter]] = None
    override def OR: Option[Seq[AFilter]]  = None
    override def NOT: Option[AFilter]      = None
  }

  case class BFilter(
      field1: Option[LongFilter],
      field2: Option[StringFilter]
  ) extends EntityFilter[BFilter] {
    override def AND: Option[Seq[BFilter]] = None
    override def OR: Option[Seq[BFilter]]  = None
    override def NOT: Option[BFilter]      = None
  }

  implicit val filterB: TableFilter[B, BFilter] = TableFilter.derive[B, BFilter]()
  implicit val filterA: TableFilter[A, AFilter] = TableFilter.derive[A, AFilter]()

  val bFilter = BFilter(
    Some(LongFilter.empty.copy(EQ = Some(1L))),
    Some(StringFilter.empty)
  )
  val aFilter = AFilter(
    None,
    None,
    Some(RelationFilter[A, B, BFilter](Some(bFilter), None, None))
  )

  //val a: List[Option[Fragment]] = filterA.relationFilterFragments(aFilter)
  //println(a)

  println(
    LabelledGeneric[AFilter]
      .to(aFilter)
      .flatMap(extractRelationFilter)
      .map(relationFilterToOptionFragment)
      .toList[Option[String] => Option[Fragment]]
      .map(_.apply(None))
  )
  //val test = Derivator[A, AFilter]().make.relationValues(LabelledGeneric[AFilter].to(aFilter))
  //println(test)
}
