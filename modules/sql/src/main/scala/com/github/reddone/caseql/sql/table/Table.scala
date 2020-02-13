package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.util.StringUtils
import doobie._
import shapeless.{HList, LabelledGeneric, Lazy, ops}

import scala.language.dynamics
import scala.reflect.runtime.universe.{Symbol => _, _}

trait Table[A, K] extends TableQuery[A, K] { self =>

  def name: String

  def schema: Option[String]

  def fieldConverter: Map[String, String]

  def fieldMapper(field: String): String

  def fields: List[String]

  def keyFields: List[String]

  implicit def read: Read[A]

  implicit def write: Write[A]

  implicit def keyRead: Read[K]

  implicit def keyWrite: Write[K]

  def alias: String

  def syntax: TableSyntax[A]
}

object Table {

  implicit val unit: Table[Unit, Unit] = derive[Unit, Unit]()

  def apply[A, K](implicit ev: Table[A, K]): Table[A, K] = ev

  object derive {

    def apply[A, K] = new Partial[A, K]

    class Partial[A, K] {

      def apply[ReprA <: HList, KeysA <: HList, ReprK <: HList, KeysK <: HList](
          aName: Option[String] = None,
          aSchema: Option[String] = None,
          aFieldConverter: Map[String, String] = Map.empty[String, String],
          aFieldMapper: String => String = StringUtils.camelToSnake,
          useTableAlias: Boolean = true
      )(
          implicit
          tag: TypeTag[A],
          lgenA: LabelledGeneric.Aux[A, ReprA],
          readA: Read[A],
          writeA: Write[A],
          keysA: ops.record.Keys.Aux[ReprA, KeysA],
          toListKeysA: Lazy[ops.hlist.ToList[KeysA, Symbol]],
          lgenK: LabelledGeneric.Aux[K, ReprK],
          readK: Read[K],
          writeK: Write[K],
          keysK: ops.record.Keys.Aux[ReprK, KeysK],
          toListKeysK: Lazy[ops.hlist.ToList[KeysK, Symbol]],
          extractorAK: ops.record.Extractor[ReprA, ReprK]
      ): Table[A, K] = new Table[A, K] { self =>

        private val tpeName = typeOf[A].typeSymbol.name.toString

        override val name: String = aName.getOrElse(StringUtils.camelToSnake(tpeName))

        override val schema: Option[String] = aSchema

        override val fieldConverter: Map[String, String] = aFieldConverter

        override def fieldMapper(field: String): String = aFieldMapper(field)

        override val fields: List[String] = keysA().toList(toListKeysA.value).map(_.name)

        override val keyFields: List[String] = keysK().toList(toListKeysK.value).map(_.name)

        override implicit val read: Read[A] = readA

        override implicit val write: Write[A] = writeA

        override implicit val keyRead: Read[K] = readK

        override implicit val keyWrite: Write[K] = writeK

        override val alias: String = TableRegistrar.aliasFor(tpeName)

        override val syntax: TableSyntax[A] =
          if (useTableAlias) TableSyntax(Some(alias), self) else TableSyntax(None, self)
      }
    }
  }
}
