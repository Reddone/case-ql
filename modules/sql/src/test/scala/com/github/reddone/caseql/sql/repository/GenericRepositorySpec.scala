package com.github.reddone.caseql.sql.repository

import doobie._
import doobie.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GenericRepositorySpec extends AnyFlatSpec with Matchers {
  import GenericDDL._
  import GenericDML._

  val schemaName: String   = "public"
  val tableName: String    = "product"
  val sequenceName: String = "product_id"

  "A CommonRepository ddl" should "produce the correct fragment when creating a schema" in {
    val frag1 = create_schema_ddl(schemaName, checkExistence = true)
    val frag2 = create_schema_ddl(schemaName, checkExistence = false)

    val expectedSql1 = s"CREATE SCHEMA IF NOT EXISTS $schemaName "
    val expectedSql2 = s"CREATE SCHEMA $schemaName "

    frag1.update.sql shouldBe expectedSql1
    frag2.update.sql shouldBe expectedSql2
  }

  it should "produce the correct fragment when deleting a schema" in {
    val frag1 = drop_schema_ddl(schemaName, checkExistence = true)
    val frag2 = drop_schema_ddl(schemaName, checkExistence = false)
  }

  it should "produce the correct fragment when creating a table" in {
    val frag1 = create_table_ddl(tableName, Some(schemaName), "", checkExistence = true)
    val frag2 = create_table_ddl(tableName, Some(schemaName), "", checkExistence = false)
  }

  it should "produce the correct fragment when deleting a table" in {
    val frag1 = drop_table_ddl(tableName, Some(schemaName), checkExistence = true)
    val frag2 = drop_table_ddl(tableName, Some(schemaName), checkExistence = false)
  }

  it should "produce the correct fragment when creating a sequence" in {
    val frag1 = create_sequence_ddl(sequenceName, Some(schemaName), "", checkExistence = true)
    val frag2 = create_sequence_ddl(sequenceName, Some(schemaName), "", checkExistence = false)
  }

  it should "produce the correct fragment when deleting a sequence" in {
    val frag1 = drop_sequence_ddl(sequenceName, Some(schemaName), checkExistence = true)
    val frag2 = drop_sequence_ddl(sequenceName, Some(schemaName), checkExistence = false)
  }

  "A CommonRepository dml" should "produce the correct fragment when selecting from a table" in {}

  it should "produce the correct fragment when inserting in a table" in {}

  it should "produce the correct fragment when updating a table" in {}

  it should "produce the correct fragment when deleting a table" in {}
}
