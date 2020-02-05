package com.github.reddone.caseql.sql.generic

import java.io.FilterWriter

import cats.data.NonEmptyList
import cats.implicits._
import com.github.reddone.caseql.sql.filter.EntityFilter
import com.github.reddone.caseql.sql.filter.models.{Filter, IntFilter, LongFilter, StringFilter}
import com.github.reddone.caseql.sql.generic.Table.{Aux, derive}
import com.github.reddone.caseql.sql.generic.TableFunction.extractFilter
import com.github.reddone.caseql.sql.modifier.models.Modifier
import com.github.reddone.caseql.sql.util.FragmentUtils
import doobie._
import doobie.implicits._
import io.circe.Decoder
import shapeless.labelled.FieldType
import shapeless.ops.hlist.Tupler
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Poly1, Witness, ops}
import shapeless.labelled.{FieldType, field}
import shapeless.{::, HNil, Poly1, Witness}

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
// experiment to introduce batch insert, update, delete
// final case class GraphFilter(filter, union: Option[Seq[GraphFIlter]], expect)
// final case class GraphUpdater(modifier, filter, batch: Option[Seq[Updater]])
// final case class GraphInserter(modifier)
// final case class GraphDeleter(filter, batch: )

//final case class EntityFilter[T](
//    t: Option[T],
//    AND: Option[Seq[EntityFilter[T]]],
//    OR: Option[Seq[EntityFilter[T]]],
//    NOT: Option[EntityFilter[T]]
//)
//
//object Test {
//  class A
//
//  val wrap = EntityFilter(None, AND = Some(Seq(EntityFilter(Some(new A()), None, None, None))), None, None)
//}
//
//final case class RelationFilter[T](
//    EVERY: Option[EntityFilter[T]],
//    SOME: Option[EntityFilter[T]],
//    NONE: Option[EntityFilter[T]]
//)

// rel filter

final case class RelFilter[A, B, T <: EntityFilter[T]](
    EVERY: Option[T],
    SOME: Option[T],
    NONE: Option[T]
)

object RelFilter {
  implicit def decoder[A, B, T <: EntityFilter[T]: Decoder]: Decoder[RelFilter[A, B, T]] =
    io.circe.generic.semiauto.deriveDecoder[RelFilter[A, B, T]]
}

// relation

//sealed trait Relation[A, B] {
//  def toFragment: Fragment = Fragment.empty
//  def inverse: Relation[B, A]
//}
//
//object Relation {}
//
//trait SingleRelation[A, B] extends Relation[A, B]
//
//trait MultiRelation[A, B] extends Relation[A, B]
//
//case class OneToOneRel[A, B](condition: (Table[A], Table[B]) => NonEmptyList[(String, String)])(
//    implicit
//    tableA: Table[A],
//    tableB: Table[B]
//) extends SingleRelation[A, B] {
//  // JOIN scheam.tableB ON tableA._1 = tableB._1 AND tableA._2 = tableB._2
//  condition(tableA, tableB)
//    .map {
//      case (left, right) => s"${tableA.defaultSyntax.column(left)} = ${tableB.defaultSyntax.column(right)}"
//    }
//    .toList
//    .mkString(" AND ")
//
//  override def inverse: Relation[B, A] = OneToOneRel[B, A]((b, a) => condition(a, b).map(_.swap))
//}
//
//case class OneToOneOptRel[A, B](condition: (Table[A], Table[B]) => NonEmptyList[(String, String)])(
//    implicit
//    tableA: Table[A],
//    tableB: Table[B]
//) extends SingleRelation[A, B] {
//  // LEFT JOIN scheam.tableB ON tableA._1 = tableB._1 AND tableA._2 = tableB._2
//  condition(tableA, tableB)
//    .map {
//      case (left, right) => s"${tableA.defaultSyntax.column(left)} = ${tableB.defaultSyntax.column(right)}"
//    }
//    .toList
//    .mkString(" AND ")
//
//  override def inverse: Relation[B, A] = OneToOneOptRel[B, A]((b, a) => condition(a, b).map(_.swap))
//}
//
//case class ManyToOneRelation[A, B]() extends SingleRelation[A, B] {
//  override def inverse: Relation[B, A] = ???
//}
//
//case class ManyToOneOptRelation[A, B]() extends SingleRelation[A, B] {
//  override def inverse: Relation[B, A] = ???
//}
//
//case class OneToManyRelation[A, B]() extends MultiRelation[A, B] {
//  override def inverse: Relation[B, A] = ???
//}
//
//case class OneToManyOptRelation[A, B]() extends MultiRelation[A, B] {
//  override def inverse: Relation[B, A] = ???
//}

// extract all relation filters

object extractRelationFilter extends TableFunction.extract[Option[RelFilter[_, _, _]]]

object relationFilterToFragment extends Poly1 {
  implicit def atOptionRelFilter[A, B, T <: EntityFilter[T], K <: Symbol, V <: Option[RelFilter[A, B, T]]](
      implicit
      wt: Witness.Aux[K],
      link: Link[A, B],
      tableFilter: TableFilterRel[B, T]
  ): Case.Aux[FieldType[K, V], FieldType[K, Option[Fragment]]] =
    at[FieldType[K, V]] { ft =>
      //implicit val table = relation.tableB
      //field[K](ft.flatMap(f => FilterWrapper.filterFragment[B, T](f.filter)))
      field[K](
        ft.flatMap(f =>
          if (link.isJunction) {
            // Single relation is implemented using a junction table
            val left      = link.tableA.syntax("l")
            val right     = link.tableB.syntax("r")
            val middle    = link.tableC.syntax("m")
            val leftCond  = link.leftJoinFields
            val rightCond = link.rightJoinFields
            // EVERY
            // NOT EXISTS(
            //   SELECT ONE
            //   FROM joinTable
            //   LEFT JOIN (SELECT * FROM rightTable WHERE filter) AS rightTable
            //   ON jointable.id = rightTable.id
            //   WHERE joinTable.id = leftTable.id AND IS NULL rightTable.id
            // )

            val a = (a: Table[A]#Syntax) => 3

            a(link.tableA.defaultSyntax)

            val leftCondSql = leftCond
              .map {
                case (lf, rf) => s"${left.column(lf)} = ${middle.column(rf)}"
              }
              .mkString(" AND ")
            val rightCondSql = rightCond
              .map {
                case (lf, rf) => s"${right.column(lf)} = ${middle.column(rf)}"
              }
              .mkString(" AND ")
            val rightIsNull = rightCond.map(_._1).map(r => s"IS NULL $r").mkString(" AND ")
            val sqlString = (filterFrag: Fragment) =>
              Fragment.const(s"""
              |NOT EXISTS (
              |SELECT ONE
              |FROM ${middle.name} LEFT JOIN ${right.name}
              |ON ${rightCondSql}
              |WHERE ${leftCondSql} AND $rightIsNull AND
              |""".stripMargin) ++ filterFrag ++
                Fragment.const(")")

            f.EVERY.flatMap(ff => EntityFilter2.filterFragment[B, T](ff, right)(tableFilter)).map(sqlString)
            // SOME
            // (YOU CAN USE LEFT JOIN AND ADD "IS NOT NULL rightTable.id") - Contrary of NONE
            // EXISTS(
            //   SELECT ONE
            //   FROM joinTable
            //   INNER JOIN (SELECT * FROM rightTable WHERE filter) AS rightTable
            //   ON jointable.id = rightTable.id
            //   WHERE joinTable.id = leftTable.id
            // )

            // NONE
            // (YOU CAN USE LEFT JOIN AND ADD "IS NOT NULL rightTable.id") - Contrary of SOME
            // NOT EXISTS(
            //   SELECT ONE
            //   FROM joinTable
            //   INNER JOIN (SELECT * FROM rightTable WHERE filter) AS rightTable
            //   ON jointable.id = rightTable.id
            //   WHERE joinTable.id = leftTable.id
            // )

            //None
          } else {
            // Single relation is implemented using a direct table
            val left     = link.tableA.defaultSyntax //.syntax("l")
            val right    = link.tableB.defaultSyntax //.syntax("r")
            val leftCond = link.leftJoinFields
            val leftCondSql = leftCond
              .map {
                case (lf, rf) => s"${left.column(lf)} = ${right.column(rf)}"
              }
              .mkString(" AND ")
            // EVERY
            // EXISTS(
            //  SELECT ONE WHERE
            //  (SELECT COUNT(*) FROM (SELECT * FROM rightTable WHERE filter)
            //    as rightTable WHERE rightTable.id = leftTable.id)
            //   =
            //   (SELECT COUNT(*) FROM rightTable WHERE rightTable.id = leftTable.id
            // )

            // SOME
            // EXISTS(
            //   SELECT ONE
            //   FROM (SELECT * FROM rightTable WHERE filter) as rightTable
            //   WHERE rightTable.id = leftTable.id
            // )
            val sqlString = (filterFrag: Fragment) =>
              Fragment.const(s"""
                                |EXISTS (
                                |SELECT ONE
                                |FROM ${right.name}
                                |WHERE ${leftCondSql} AND
                                |""".stripMargin) ++ filterFrag ++
                Fragment.const(")")

            f.EVERY.flatMap(ff => EntityFilter2.filterFragment[B, T](ff, right)(tableFilter)).map(sqlString)
            // NONE
            // NOT EXISTS(
            //   SELECT ONE
            //   FROM (SELECT * FROM rightTable WHERE filter) as rightTable
            //   WHERE rightTable.id = leftTable.id
            // )
          }
        )
      )
    }
}

object Test extends App {

  case class A(field1: String, field2: Int)
  case class AKey(field1: String)
  case class B(field1: Long, field2: String)
  case class BKey(field1: Long)

  implicit val tableA: Aux[A, AKey] = Table.derive[A, AKey]()
  implicit val tableB: Aux[B, BKey] = Table.derive[B, BKey]()

  implicit val relAB: Link[A, B] =
    Link.direct[A, B]((a, b) => NonEmptyList.of(("field1", "field2")))

  case class AFilter(
      field1: Option[StringFilter],
      field2: Option[IntFilter],
      bRelation: Option[RelFilter[A, B, BFilter]]
  ) extends EntityFilter[AFilter] {
    override def AND: Option[Seq[AFilter]] = None
    override def OR: Option[Seq[AFilter]]  = None
    override def NOT: Option[AFilter]      = None
  }

  implicit val decoder: Decoder[AFilter] = io.circe.generic.semiauto.deriveDecoder[AFilter]

  case class BFilter(
      field1: Option[LongFilter],
      field2: Option[StringFilter]
  ) extends EntityFilter[BFilter] {
    override def AND: Option[Seq[BFilter]] = None
    override def OR: Option[Seq[BFilter]]  = None
    override def NOT: Option[BFilter]      = None
  }

  implicit val decoder1: Decoder[BFilter] = io.circe.generic.semiauto.deriveDecoder[BFilter]

  implicit val filter2: TableFilterRel[B, BFilter] = TableFilterRel.derive[B, BFilter]()
  implicit val filter: TableFilterRel[A, AFilter]  = TableFilterRel.derive[A, AFilter]()

  val bFilter = BFilter(
    Some(LongFilter.empty.copy(EQ = Some(1L))),
    Some(StringFilter.empty)
  )
  val aFilter = AFilter(
    None,
    None,
    Some(RelFilter[A, B, BFilter](Some(bFilter), None, None))
  )
  println(
    LabelledGeneric[AFilter]
      .to(aFilter)
      .flatMap(extractRelationFilter)
      .map(relationFilterToFragment)
      .toList[Option[Fragment]]
  )

  val test = Derivator[AFilter]().make.relationValues(LabelledGeneric[AFilter].to(aFilter))
  println(test)
}

// alternative

trait TableFilterRel[T, U] {
  def keys(): List[Symbol]
  def values(u: U): List[Option[Filter[_]]]
  def relations(u: U): List[Option[Fragment]]

  def andCombinator(u: U): Option[Seq[U]] = None
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
          tableFilterLR: Lazy[ReprTableFilterRel.Aux[L, R, RKeys, RValues]],
          keyToTraversableR: ops.hlist.ToTraversable.Aux[RKeys, List, Symbol],
          valueToTraversableR: ops.hlist.ToTraversable.Aux[RValues, List, Option[Filter[_]]]
      ): TableFilterRel[T, U] = new TableFilterRel[T, U] {
        override def keys(): List[Symbol]                    = tableFilterLR.value.keys().toList
        override def values(u: U): List[Option[Filter[_]]]   = tableFilterLR.value.values(lgenU.to(u)).toList
        override def relations(u: U): List[Option[Fragment]] = tableFilterLR.value.relationValues(lgenU.to(u))
      }
    }
  }
}

trait ReprTableFilterRel[L <: HList, R <: HList] {
  type Keys <: HList
  type Values <: HList

  def keys(): Keys
  def values(r: R): Values
  def relationValues(r: R): List[Option[Fragment]]
}

object ReprTableFilterRel {
  type OptionFilter[A] = Option[Filter[A]]

  type Aux[L <: HList, R <: HList, Keys0 <: HList, Values0 <: HList] =
    ReprTableFilterRel[L, R] {
      type Keys   = Keys0
      type Values = Values0
    }

  //val and =

  implicit def tableFilter[
      L <: HList,
      LKeys <: HList,
      LValues <: HList,
      LValuesWrapped <: HList,
      LZipped <: HList,
      R <: HList,
      RFilter <: HList,
      RFilterKeys <: HList,
      RFilterValues <: HList,
      RAligned <: HList
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
      rel: ReprRelationFilter[R]
  ): Aux[L, R, RFilterKeys, RFilterValues] = new ReprTableFilterRel[L, R] {
    override type Keys   = RFilterKeys
    override type Values = RFilterValues

    override def keys(): RFilterKeys                          = unzippedR.keys()
    override def values(r: R): RFilterValues                  = unzippedR.values(r.flatMap(extractFilter))
    override def relationValues(r: R): List[Option[Fragment]] = rel.relationValues(r)
  }
}

case class Derivator[T]() {

  implicit def make[L <: HList](
      implicit
      lgenT: LabelledGeneric.Aux[T, L],
      rep: ReprRelationFilter[L]
  ): ReprRelationFilter[L] = rep
}

trait IsEntityFilter[L, RFilter] {}

object IsEntityFilter {}

trait ReprRelationFilter[R <: HList] {
  def relationValues(r: R): List[Option[Fragment]]
}

object ReprRelationFilter {

  implicit def derive[
      R <: HList,
      RRelFilter <: HList,
      RRelMapped <: HList
  ](
      implicit
      extractRelFilterTR: ops.hlist.FlatMapper.Aux[extractRelationFilter.type, R, RRelFilter],
      mapper: ops.hlist.Mapper.Aux[relationFilterToFragment.type, RRelFilter, RRelMapped],
      toList: ops.hlist.ToList[RRelMapped, Option[Fragment]]
  ): ReprRelationFilter[R] = new ReprRelationFilter[R] {
    override def relationValues(r: R): List[Option[Fragment]] =
      r.flatMap(extractRelationFilter)(extractRelFilterTR)
        .map(relationFilterToFragment)(mapper)
        .toList(toList)
  }
}

object EntityFilter2 {

  def filterFragment[T, U <: EntityFilter[U]](filter: U, syntax: Table[T]#Syntax)(
      implicit
      tableFilter: TableFilterRel[T, U]
  ): Option[Fragment] = {
    def make(filter: U): Option[Fragment] = {
      val left  = tableFilter.keys().map(_.name).map(syntax.column)
      val right = tableFilter.values(filter)
      val zipped = left.zip(right).map {
        case (col, optionFilter) => optionFilter.flatMap(_.toOptionFragment(col))
      }
      FragmentUtils.optionalAndOpt(zipped: _*)
    }

    def makeRelations(filter: U): Option[Fragment] = {
      FragmentUtils.optionalAndOpt(tableFilter.relations(filter): _*)
    }

    FragmentUtils.optionalAndOpt(
      make(filter),
      makeRelations(filter),
      filter.AND.flatMap { and =>
        val recs = and.map(filterFragment(_, syntax))
        FragmentUtils.optionalAndOpt(recs: _*)
      },
      filter.OR.flatMap { or =>
        val recs = or.map(filterFragment(_, syntax))
        FragmentUtils.optionalOrOpt(recs: _*)
      },
      filter.NOT.flatMap { not =>
        val rec = filterFragment(not, syntax)
        FragmentUtils.optionalNot(rec)
      }
    )
  }
}
