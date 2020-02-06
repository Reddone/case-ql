package com.github.reddone.caseql.sql

import java.sql.Timestamp

import com.github.reddone.caseql.sql.filter.models._
import com.github.reddone.caseql.sql.generic.{Table, TableFilter, TableModifier}
import com.github.reddone.caseql.sql.modifier.models._
import doobie._
import doobie.implicits._
import Fragment._
import com.github.reddone.caseql.sql.filter.wrappers.EntityFilter
import com.github.reddone.caseql.sql.modifier.wrappers.EntityModifier
import javasql._
import javatime._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SpecPlayground extends AnyFlatSpec with Matchers {

  // test model
  case class Test(
      field1: Int,
      field2: Option[String],
      field3: Long,
      field4: Option[Timestamp]
  )
  // test key
  case class TestKey(
      field1: Int,
      field3: Long
  )
  // test filter
  case class TestFilter(
      field1: Option[IntFilter],
      field2: Option[StringFilterOption],
      field3: Option[LongFilter],
      field4: Option[TimestampFilterOption],
      AND: Option[Seq[TestFilter]],
      OR: Option[Seq[TestFilter]],
      NOT: Option[TestFilter]
  ) extends EntityFilter[TestFilter]
  // test modifier
  case class TestModifier(
      field1: Option[IntModifier],
      field2: Option[StringModifierOption],
      field3: Option[LongModifier],
      field4: Option[TimestampModifierOption]
  ) extends EntityModifier[TestModifier]

  implicit val table: Table[Test]                               = Table.derive[Test, TestKey]()
  implicit val tableFilter: TableFilter[Test, TestFilter]       = TableFilter.derive[Test, TestFilter]()
  implicit val tableModifier: TableModifier[Test, TestModifier] = TableModifier.derive[Test, TestModifier]()

  "SpecPlayground" should "do anything" in {}
}
