package com.github.reddone.caseql.sql.table

import com.github.reddone.caseql.sql.util.StringUtils
import doobie._
import shapeless.{HList, LabelledGeneric, Lazy, ops}

import scala.language.dynamics
import scala.reflect.runtime.universe.{Symbol => _, _}

trait Table[T, K] extends TableQuery[T, K] { self =>

  def name: String

  def schema: Option[String]

  def fieldConverter: Map[String, String]

  def fieldMapper(field: String): String

  def fields: List[String]

  def keyFields: List[String]

  implicit def read: Read[T]

  implicit def write: Write[T]

  implicit def keyRead: Read[K]

  implicit def keyWrite: Write[K]

  def alias: String

  def syntax: TableSyntax[T]
}

object Table {

  implicit val unit: Table[Unit, Unit] = derive[Unit, Unit]()

  def apply[T, K](implicit table: Table[T, K]): Table[T, K] = table

  object derive {

    def apply[T, K] = new Partial[T, K]

    class Partial[T, K] {

      def apply[ReprT <: HList, KeysT <: HList, ReprK <: HList, KeysK <: HList](
          aName: Option[String] = None,
          aSchema: Option[String] = None,
          aFieldConverter: Map[String, String] = Map.empty[String, String],
          aFieldMapper: String => String = StringUtils.camelToSnake,
          useTableAlias: Boolean = true
      )(
          implicit
          tag: TypeTag[T],
          lgenT: LabelledGeneric.Aux[T, ReprT],
          readT: Read[T],
          writeT: Write[T],
          keysT: ops.record.Keys.Aux[ReprT, KeysT],
          toListKeysT: Lazy[ops.hlist.ToList[KeysT, Symbol]],
          lgenK: LabelledGeneric.Aux[K, ReprK],
          readK: Read[K],
          writeK: Write[K],
          keysK: ops.record.Keys.Aux[ReprK, KeysK],
          toListKeysK: Lazy[ops.hlist.ToList[KeysK, Symbol]],
          extractorTK: ops.record.Extractor[ReprT, ReprK]
      ): Table[T, K] = new Table[T, K] { self =>

        private val tpeName = typeOf[T].typeSymbol.name.toString

        override val name: String = aName.getOrElse(StringUtils.camelToSnake(tpeName))

        override val schema: Option[String] = aSchema

        override val fieldConverter: Map[String, String] = aFieldConverter

        override def fieldMapper(field: String): String = aFieldMapper(field)

        override val fields: List[String] = keysT().toList(toListKeysT.value).map(_.name)

        override val keyFields: List[String] = keysK().toList(toListKeysK.value).map(_.name)

        override implicit val read: Read[T] = readT

        override implicit val write: Write[T] = writeT

        override implicit val keyRead: Read[K] = readK

        override implicit val keyWrite: Write[K] = writeK

        override val alias: String = TableRegistrar.aliasFor(tpeName)

        override val syntax: TableSyntax[T] = if (useTableAlias) TableSyntax(alias, self) else TableSyntax("", self)
      }
    }
  }
}
