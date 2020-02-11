package com.github.reddone.caseql.sql

import com.github.reddone.caseql.sql.TestModel._
import com.github.reddone.caseql.sql.table.{Table, TableFilter, TableModifier}
import doobie._
import doobie.implicits._
import javasql._
import javatime._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SpecPlayground extends AnyFlatSpec with Matchers {

  implicit val table: Table[Test, TestKey]                      = Table.derive[Test, TestKey]()
  implicit val tableFilter: TableFilter[Test, TestFilter]       = TableFilter.derive[Test, TestFilter]()
  implicit val tableModifier: TableModifier[Test, TestModifier] = TableModifier.derive[Test, TestModifier]()

  "SpecPlayground" should "do anything" in {}
}
