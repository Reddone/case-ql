package com.github.reddone.caseql.sql

import com.github.reddone.caseql.sql.TestModel._
import com.github.reddone.caseql.sql.filter.models.{IntFilter, LongFilter, StringFilter}
import com.github.reddone.caseql.sql.filter.wrappers.{EntityFilter, RelationFilter}
import com.github.reddone.caseql.sql.table.TableLink.Aux
import com.github.reddone.caseql.sql.table.{FieldSet, Table, TableFilter, TableLink}
import doobie._
import doobie.implicits._
import javasql._
import javatime._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SpecPlayground extends AnyFlatSpec with Matchers {

  "SpecPlayground" should "do anything" in {
    // tables
    implicit val leftTable: Table[TestLeft, TestLeftKey]             = Table.derive[TestLeft, TestLeftKey]()
    implicit val directTable: Table[TestDirect, TestDirectKey]       = Table.derive[TestDirect, TestDirectKey]()
    implicit val rightTable: Table[TestRight, TestRightKey]          = Table.derive[TestRight, TestRightKey]()
    implicit val junctionTable: Table[TestJunction, TestJunctionKey] = Table.derive[TestJunction, TestJunctionKey]()
    // links
    implicit val leftSelfLink: Aux[TestLeft, TestLeft, Unit] = TableLink.self[TestLeft](
      FieldSet("field1"),
      FieldSet("field3")
    )
    implicit val directLeftLink: Aux[TestDirect, TestLeft, Unit] = TableLink.direct[TestDirect, TestLeft](
      FieldSet("field3"),
      FieldSet("field1")
    )
    implicit val leftJunctionLink: Aux[TestLeft, TestJunction, Unit] = TableLink.direct[TestLeft, TestJunction](
      FieldSet("field1"),
      FieldSet("field1")
    )
    implicit val rightJunctionLink: Aux[TestRight, TestJunction, Unit] = TableLink.direct[TestRight, TestJunction](
      FieldSet("field1"),
      FieldSet("field2")
    )
    implicit val leftRightLink: Aux[TestLeft, TestRight, TestJunction] =
      TableLink.union(leftJunctionLink, rightJunctionLink)

    // table filters
    implicit lazy val rightTableFilter: TableFilter[TestRight, TestRightFilter] =
      TableFilter.derive[TestRight, TestRightFilter]()
    implicit lazy val leftTableFilter: TableFilter[TestLeft, TestLeftFilter] =
      TableFilter.derive[TestLeft, TestLeftFilter]()
  }
}
