package com.github.reddone.caseql.sql.generic

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

  final def syntax(alias: String): Syntax = Syntax(alias, self)

  final val defaultSyntax: Syntax = Syntax("", self)

  sealed case class Syntax(alias: String, support: self.type) extends Dynamic {

    private val aliasO = if (alias.isEmpty) None else Some(alias)

    lazy val name: String = {
      val fullName        = StringUtils.addPrefix(support.name, support.schema)
      val aliasedFullName = StringUtils.addSuffix(fullName, aliasO, " ")
      aliasedFullName
    }

    lazy val columns: List[String] = support.fields.map(c).map(StringUtils.addPrefix(_, aliasO))

    lazy val keyColumns: List[String] = support.keyFields.map(c).map(StringUtils.addPrefix(_, aliasO))

    def column(field: String): String = StringUtils.addPrefix(c(field), aliasO)

    def selectDynamic(field: String): String = StringUtils.addPrefix(c(field), aliasO)

    private def c(field: String): String = support.fieldConverter.getOrElse(field, support.fieldMapper(field))
  }
}

object Table {

  def apply[T, K](implicit ev: Table[T, K]): Table[T, K] = ev

  object derive {

    def apply[T, K] = new Partial[T, K]

    class Partial[T, K] {

      def apply[L <: HList, LKeys <: HList, R <: HList, RKeys <: HList](
          aName: Option[String] = None,
          aSchema: Option[String] = None,
          aFieldConverter: Map[String, String] = Map.empty[String, String],
          aFieldMapper: String => String = StringUtils.camelToSnake
      )(
          implicit
          tag: TypeTag[T],
          lgenT: LabelledGeneric.Aux[T, L],
          readT: Read[T],
          writeT: Write[T],
          keysL: ops.record.Keys.Aux[L, LKeys],
          toListL: Lazy[ops.hlist.ToList[LKeys, Symbol]],
          lgenK: LabelledGeneric.Aux[K, R],
          readK: Read[K],
          writeK: Write[K],
          keysR: ops.record.Keys.Aux[R, RKeys],
          toListR: Lazy[ops.hlist.ToList[RKeys, Symbol]],
          extractorLR: ops.record.Extractor[L, R]
      ): Table[T, K] = new Table[T, K] {

        override val name: String = aName.getOrElse(StringUtils.camelToSnake(typeOf[T](tag).typeSymbol.name.toString))

        override val schema: Option[String] = aSchema

        override val fieldConverter: Map[String, String] = aFieldConverter

        override def fieldMapper(field: String): String = aFieldMapper(field)

        override val fields: List[String] = keysL().toList(toListL.value).map(_.name)

        override val keyFields: List[String] = keysR().toList(toListR.value).map(_.name)

        override implicit val read: Read[T] = readT

        override implicit val write: Write[T] = writeT

        override implicit val keyRead: Read[K] = readK

        override implicit val keyWrite: Write[K] = writeK
      }
    }
  }
}
