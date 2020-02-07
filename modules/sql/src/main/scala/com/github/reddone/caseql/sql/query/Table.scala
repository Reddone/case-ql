package com.github.reddone.caseql.sql.query

import java.util.concurrent.atomic.AtomicLong

import com.github.reddone.caseql.sql.util.{FragmentUtils, StringUtils}
import doobie._
import shapeless.{HList, LabelledGeneric, Lazy, ops}

import scala.collection.concurrent.TrieMap
import scala.language.dynamics
import scala.reflect.runtime.universe.{Symbol => _, _}

trait Table[T, K] extends TableQuery[T, K] { self =>

  def name: String

  def shortenedName: String

  def schema: Option[String]

  def fieldConverter: Map[String, String]

  def fieldMapper(field: String): String

  def fields: List[String]

  def keyFields: List[String]

  implicit def read: Read[T]

  implicit def write: Write[T]

  implicit def keyRead: Read[K]

  implicit def keyWrite: Write[K]

  final def syntax(alias: String): Syntax[T] = Syntax(alias, self)

  private[query] final lazy val defaultSyntax: Syntax[T] = Syntax(shortenedName, self)
}

// Syntax decouples a table from its key
final case class Syntax[T](alias: String, support: Table[T, _]) extends Dynamic {

  private val aliasO = if (alias.isEmpty) None else Some(alias)

  lazy val name: String = {
    val fullName        = StringUtils.addPrefix(support.name, support.schema)
    val aliasedFullName = StringUtils.addSuffix(fullName, aliasO, " ")
    if (alias == support.name) fullName else aliasedFullName
  }

  lazy val columns: List[String] = support.fields.map(c).map(StringUtils.addPrefix(_, aliasO))

  lazy val keyColumns: List[String] = support.keyFields.map(c).map(StringUtils.addPrefix(_, aliasO))

  def column(field: String): String = StringUtils.addPrefix(c(field), aliasO)

  def selectDynamic(field: String): String = StringUtils.addPrefix(c(field), aliasO)

  private def c(field: String): String = support.fieldConverter.getOrElse(field, support.fieldMapper(field))
}

object Table {

  private val counter = new AtomicLong(0L)

  private val tableRegister = new TrieMap[String, String]()

  // move register in a TableRegistrar

  def apply[T, K](implicit ev: Table[T, K]): Table[T, K] = ev

  implicit val unit: Table[Unit, Unit] = derive[Unit, Unit]()

  object derive {

    def apply[T, K] = new Partial[T, K]

    class Partial[T, K] {

      // TODO: singleton ???
      def apply(implicit ev: Table[T, K]): Table[T, K] = ev

      def apply[ReprT <: HList, KeysT <: HList, ReprK <: HList, KeysK <: HList](
          aName: Option[String] = None,
          aSchema: Option[String] = None,
          aFieldConverter: Map[String, String] = Map.empty[String, String],
          aFieldMapper: String => String = StringUtils.camelToSnake
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
      ): Table[T, K] = new Table[T, K] { table =>

        override val name: String = aName.getOrElse(StringUtils.camelToSnake(typeOf[T](tag).typeSymbol.name.toString))

        override val schema: Option[String] = aSchema

        override val shortenedName: String = {
          val a = StringUtils.shorten(name) + counter.getAndIncrement().toString
          tableRegister.put(name, a)
          a
        }
        //tableRegister.getOrElseUpdate(
        //name,
        //StringUtils.shorten(name) + counter.getAndIncrement().toString
        //)

        override val fieldConverter: Map[String, String] = aFieldConverter

        override def fieldMapper(field: String): String = aFieldMapper(field)

        override val fields: List[String] = keysT().toList(toListKeysT.value).map(_.name)

        override val keyFields: List[String] = keysK().toList(toListKeysK.value).map(_.name)

        override implicit val read: Read[T] = readT

        override implicit val write: Write[T] = writeT

        override implicit val keyRead: Read[K] = readK

        override implicit val keyWrite: Write[K] = writeK
      }
    }
  }
}
