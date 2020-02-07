package com.github.reddone.caseql.sql.query

import cats.data.NonEmptyList
import com.github.reddone.caseql.sql.filter.models.{IntFilter, LongFilter, StringFilter}
import com.github.reddone.caseql.sql.filter.wrappers.{EntityFilter, RelationFilter}
import com.github.reddone.caseql.sql.query.TableFunction.{extractRelationFilter, relationFilterToOptionFragment}
import doobie._
import io.circe.Decoder
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

//object extractRelationFilter extends TableFunction.extract[Option[RelationFilter[_, _, _]]]
//
//object relationFilterToFragment extends Poly1 {
//  implicit def atOptionRelFilter[A, B, FA <: EntityFilter2[A, FA], FB <: EntityFilter2[B, FB], K <: Symbol, V <: Option[
//    EntityFilter2[A, FA]#EntityRelationFilter2[B, FB]
//  ]](
//      implicit
//      wt: Witness.Aux[K],
//      link: Link[A, B],
//      tableFilter: TableFilterRel[B, FB]
//  ): Case.Aux[FieldType[K, V], FieldType[K, Option[Fragment]]] =
//    at[FieldType[K, V]] { ft =>
//      //implicit val table = relation.tableB
//      //field[K](ft.flatMap(f => FilterWrapper.filterFragment[B, T](f.filter)))
//      field[K](
//        ft.flatMap(f =>
//          if (link.isJunction) {
//            // Single relation is implemented using a junction table
//            val left      = link.tableA.syntax("l")
//            val right     = link.tableB.syntax("r")
//            val middle    = link.tableC.syntax("m")
//            val leftCond  = link.leftJoinFields
//            val rightCond = link.rightJoinFields
//            // EVERY
//            // NOT EXISTS(
//            //   SELECT ONE
//            //   FROM joinTable
//            //   LEFT JOIN (SELECT * FROM rightTable WHERE filter) AS rightTable
//            //   ON jointable.id = rightTable.id
//            //   WHERE joinTable.id = leftTable.id AND IS NULL rightTable.id
//            // )
//
//            val a = (a: Table[A]#Syntax) => 3
//
//            a(link.tableA.defaultSyntax)
//
//            val leftCondSql = leftCond
//              .map {
//                case (lf, rf) => s"${left.column(lf)} = ${middle.column(rf)}"
//              }
//              .mkString(" AND ")
//            val rightCondSql = rightCond
//              .map {
//                case (lf, rf) => s"${right.column(lf)} = ${middle.column(rf)}"
//              }
//              .mkString(" AND ")
//            val rightIsNull = rightCond.map(_._1).map(r => s"IS NULL $r").mkString(" AND ")
//            val sqlString = (filterFrag: Fragment) =>
//              Fragment.const(s"""
//              |NOT EXISTS (
//              |SELECT ONE
//              |FROM ${middle.name} LEFT JOIN ${right.name}
//              |ON ${rightCondSql}
//              |WHERE ${leftCondSql} AND $rightIsNull AND
//              |""".stripMargin) ++ filterFrag ++
//                Fragment.const(")")
//
//            //f.EVERY.flatMap(ff => EntityFilter2.filterFragment[B, T](ff, right)(tableFilter)).map(sqlString)
//            // SOME
//            // (YOU CAN USE LEFT JOIN AND ADD "IS NOT NULL rightTable.id") - Contrary of NONE
//            // EXISTS(
//            //   SELECT ONE
//            //   FROM joinTable
//            //   INNER JOIN (SELECT * FROM rightTable WHERE filter) AS rightTable
//            //   ON jointable.id = rightTable.id
//            //   WHERE joinTable.id = leftTable.id
//            // )
//
//            // NONE
//            // (YOU CAN USE LEFT JOIN AND ADD "IS NOT NULL rightTable.id") - Contrary of SOME
//            // NOT EXISTS(
//            //   SELECT ONE
//            //   FROM joinTable
//            //   INNER JOIN (SELECT * FROM rightTable WHERE filter) AS rightTable
//            //   ON jointable.id = rightTable.id
//            //   WHERE joinTable.id = leftTable.id
//            // )
//
//            None
//          } else {
//            // we have to always re-alias the right table because of self joins
//            // We can use the same syntax inside filters by wrapping everything inside a sub query
//
//            // Single relation is implemented using a direct table
//            val left     = link.tableA.defaultSyntax //.syntax("l")
//            val right    = link.tableB.defaultSyntax //.syntax("r")
//            val leftCond = link.leftJoinFields
//            val leftCondSql = leftCond
//              .map {
//                case (lf, rf) => s"${left.column(lf)} = ${right.column(rf)}"
//              }
//              .mkString(" AND ")
//            // EVERY
//            // EXISTS(
//            //  SELECT ONE WHERE
//            //  (SELECT COUNT(*) FROM (SELECT * FROM rightTable WHERE filter)
//            //    as rightTable WHERE rightTable.id = leftTable.id)
//            //   =
//            //   (SELECT COUNT(*) FROM rightTable WHERE rightTable.id = leftTable.id
//            // )
//
//            // SOME
//            // EXISTS(
//            //   SELECT ONE
//            //   FROM (SELECT * FROM rightTable WHERE filter) as rightTable
//            //   WHERE rightTable.id = leftTable.id
//            // )
//            val sqlString = (filterFrag: Fragment) =>
//              Fragment.const(s"""
//                                |EXISTS (
//                                |SELECT ONE
//                                |FROM ${right.name}
//                                |WHERE ${leftCondSql} AND
//                                |""".stripMargin) ++ filterFrag ++
//                Fragment.const(")")
//
//            //f.EVERY.flatMap(ff => EntityFilter2.filterFragment[B, T](ff, right)(tableFilter)).map(sqlString)
//            // NONE
//            // NOT EXISTS(
//            //   SELECT ONE
//            //   FROM (SELECT * FROM rightTable WHERE filter) as rightTable
//            //   WHERE rightTable.id = leftTable.id
//            // )
//
//            None
//          }
//        )
//      )
//    }
//}

object Relation extends App {

  case class A(field1: String, field2: Int)
  case class AKey(field1: String)
  case class B(field1: Long, field2: String)
  case class BKey(field1: Long)

  implicit val tableA: Table[A, AKey] = Table.derive[A, AKey]()
  implicit val tableB: Table[B, BKey] = Table.derive[B, BKey]()

  implicit val relAB: Link[A, B] = Link.direct(tableA, tableB) { (a, b) =>
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
      .toList[Syntax[A] => Option[Fragment]]
      .map(_.apply(tableA.defaultSyntax))
  )
  //val test = Derivator[A, AFilter]().make.relationValues(LabelledGeneric[AFilter].to(aFilter))
  //println(test)
}

// alternative

//trait TableFilterRel[T, U] {
//  def keys(): List[Symbol]
//  def values(u: U): List[Option[Filter[_]]]
//  def relations(u: U): List[Option[Fragment]]
//}
//
//object TableFilterRel {
//
//  def apply[T, U](implicit ev: TableFilterRel[T, U]): TableFilterRel[T, U] = ev
//
//  object derive {
//
//    def apply[T, U] = new Partial[T, U]
//
//    class Partial[T, U] {
//
//      def apply[L <: HList, R <: HList, RKeys <: HList, RValues <: HList]()(
//          implicit
//          lgenT: LabelledGeneric.Aux[T, L],
//          lgenU: LabelledGeneric.Aux[U, R],
//          tableFilterLR: Lazy[ReprTableFilterRel.Aux[T, L, R, RKeys, RValues]],
//          keyToTraversableR: ops.hlist.ToTraversable.Aux[RKeys, List, Symbol],
//          valueToTraversableR: ops.hlist.ToTraversable.Aux[RValues, List, Option[Filter[_]]]
//      ): TableFilterRel[T, U] = new TableFilterRel[T, U] {
//        override def keys(): List[Symbol]                    = tableFilterLR.value.keys().toList
//        override def values(u: U): List[Option[Filter[_]]]   = tableFilterLR.value.values(lgenU.to(u)).toList
//        override def relations(u: U): List[Option[Fragment]] = tableFilterLR.value.relationValues(lgenU.to(u))
//      }
//    }
//  }
//}
//
//trait ReprTableFilterRel[T, L <: HList, R <: HList] {
//  type Keys <: HList
//  type Values <: HList
//
//  def keys(): Keys
//  def values(r: R): Values
//  def relationValues(r: R): List[Option[Fragment]]
//}
//
//object ReprTableFilterRel {
//  type OptionFilter[A] = Option[Filter[A]]
//
//  type Aux[T, L <: HList, R <: HList, Keys0 <: HList, Values0 <: HList] =
//    ReprTableFilterRel[T, L, R] {
//      type Keys   = Keys0
//      type Values = Values0
//    }
//
//  //val and =
//
//  implicit def tableFilter[
//  T,
//      L <: HList,
//      LKeys <: HList,
//      LValues <: HList,
//      LValuesWrapped <: HList,
//      LZipped <: HList,
//      R <: HList,
//      RFilter <: HList,
//      RFilterKeys <: HList,
//      RFilterValues <: HList,
//      RAligned <: HList
//  ](
//      implicit
//      keysL: ops.record.Keys.Aux[L, LKeys],
//      valuesL: ops.record.Values.Aux[L, LValues],
//      mappedValuesL: ops.hlist.Mapped.Aux[LValues, OptionFilter, LValuesWrapped],
//      zippedL: ops.hlist.ZipWithKeys.Aux[LKeys, LValuesWrapped, LZipped],
//      extractFilterR: ops.hlist.FlatMapper.Aux[extractFilter.type, R, RFilter],
//      unzippedR: ops.record.UnzipFields.Aux[RFilter, RFilterKeys, RFilterValues],
//      alignR: ops.record.AlignByKeys.Aux[RFilter, LKeys, RAligned],
//      extTR: <:<[RAligned, LZipped],
//      rel: ReprRelationFilter[T, R]
//  ): Aux[T, L, R, RFilterKeys, RFilterValues] = new ReprTableFilterRel[T, L, R] {
//    override type Keys   = RFilterKeys
//    override type Values = RFilterValues
//
//    override def keys(): RFilterKeys                          = unzippedR.keys()
//    override def values(r: R): RFilterValues                  = unzippedR.values(r.flatMap(extractFilter))
//    override def relationValues(r: R): List[Option[Fragment]] = rel.relationValues(r)
//  }
//}
//
//case class Derivator[A, T <: EntityFilter2[A, T]]() {
//
//  implicit def make[L <: HList](
//      implicit
//      lgenT: LabelledGeneric.Aux[T, L],
//      rep: ReprRelationFilter[A, L]
//  ): ReprRelationFilter[A, L] = rep
//}
//
//trait ReprRelationFilter[T, R <: HList] {
//  def relationValues(r: R): List[Option[Fragment]]
//}
//
//object ReprRelationFilter {
//
//  implicit def derive[
//      T,
//      RRV <: HList,
//      R <: HList,
//      RRelFilter <: HList,
//      RRelMapped <: HList
//  ](
//     implicit
//     extractRelFilterTR: ops.hlist.FlatMapper.Aux[extractRelationFilter.type, R, RRelFilter],
//     belongsToT: ops.record.Values.Aux[RRelFilter, RRV],
//     aaaa: ops.hlist.Comapped[RRV, Option[EntityFilter2[T, _]#EntityRelationFilter2[_, _]]],
//     mapper: ops.hlist.Mapper.Aux[relationFilterToFragment.type, RRelFilter, RRelMapped],
//     toList: ops.hlist.ToList[RRelMapped, Option[Fragment]]
//  ): ReprRelationFilter[T, R] = new ReprRelationFilter[T, R] {
//    override def relationValues(r: R): List[Option[Fragment]] =
//      r.flatMap(extractRelationFilter)(extractRelFilterTR)
//        .map(relationFilterToFragment)(mapper)
//        .toList(toList)
//  }
//}
//
//object EntityFilter2 {
//
//  def filterFragment[T, U <: EntityFilter[U]](filter: U, syntax: Table[T]#Syntax)(
//      implicit
//      tableFilter: TableFilterRel[T, U]
//  ): Option[Fragment] = {
//    def make(filter: U): Option[Fragment] = {
//      val left  = tableFilter.keys().map(_.name).map(syntax.column)
//      val right = tableFilter.values(filter)
//      val zipped = left.zip(right).map {
//        case (col, optionFilter) => optionFilter.flatMap(_.toOptionFragment(col))
//      }
//      FragmentUtils.optionalAndOpt(zipped: _*)
//    }
//
//    def makeRelations(filter: U): Option[Fragment] = {
//      FragmentUtils.optionalAndOpt(tableFilter.relations(filter): _*)
//    }
//
//    FragmentUtils.optionalAndOpt(
//      make(filter),
//      makeRelations(filter),
//      filter.AND.flatMap { and =>
//        val recs = and.map(filterFragment(_, syntax))
//        FragmentUtils.optionalAndOpt(recs: _*)
//      },
//      filter.OR.flatMap { or =>
//        val recs = or.map(filterFragment(_, syntax))
//        FragmentUtils.optionalOrOpt(recs: _*)
//      },
//      filter.NOT.flatMap { not =>
//        val rec = filterFragment(not, syntax)
//        FragmentUtils.optionalNot(rec)
//      }
//    )
//  }
//}
