package com.github.reddone.caseql.sql.repository

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GenericRepositorySpec extends AnyFlatSpec with Matchers {
  import GenericDDL._
  import GenericDML._

  val schemaName: String   = "public"
  val tableName: String    = "product"
  val sequenceName: String = "product_id"

  "GenericRepository DDL" should "produce the correct fragment when creating a schema" in {
    val expected1 = s"CREATE SCHEMA IF NOT EXISTS $schemaName "
    val expected2 = s"CREATE SCHEMA $schemaName "

    val result1 = create_schema_ddl(schemaName, checkExistence = true)
    val result2 = create_schema_ddl(schemaName, checkExistence = false)

    result1.update.sql shouldBe expected1
    result2.update.sql shouldBe expected2
  }

  it should "produce the correct fragment when deleting a schema" in {
    val expected1 = s"DROP SCHEMA IF EXISTS $schemaName "
    val expected2 = s"DROP SCHEMA $schemaName "

    val result1 = drop_schema_ddl(schemaName, checkExistence = true)
    val result2 = drop_schema_ddl(schemaName, checkExistence = false)

    result1.update.sql shouldBe expected1
    result2.update.sql shouldBe expected2
  }

  it should "produce the correct fragment when creating a table" in {
    val definition = """|id         BIGSERIAL PRIMARY KEY,
                        |label      VARCHAR(255) NOT NULL,
                        |created_at TIMESTAMP    NOT NULL,
                        |updated_at TIMESTAMP    NOT NULL,
                        |UNIQUE (label)""".stripMargin
    val expected1  = s"CREATE TABLE IF NOT EXISTS $schemaName.$tableName (\n" + definition + " ) "
    val expected2  = s"CREATE TABLE $schemaName.$tableName (\n" + definition + " ) "

    val result1 = create_table_ddl(tableName, Some(schemaName), definition, checkExistence = true)
    val result2 = create_table_ddl(tableName, Some(schemaName), definition, checkExistence = false)

    result1.update.sql shouldBe expected1
    result2.update.sql shouldBe expected2
  }

  it should "produce the correct fragment when deleting a table" in {
    val expected1 = s"DROP TABLE IF EXISTS $schemaName.$tableName "
    val expected2 = s"DROP TABLE $schemaName.$tableName "

    val result1 = drop_table_ddl(tableName, Some(schemaName), checkExistence = true)
    val result2 = drop_table_ddl(tableName, Some(schemaName), checkExistence = false)

    result1.update.sql shouldBe expected1
    result2.update.sql shouldBe expected2
  }

  it should "produce the correct fragment when creating a sequence" in {
    val definition = "START WITH 1 INCREMENT BY 1 NO CYCLE"
    val expected1  = s"CREATE SEQUENCE IF NOT EXISTS $schemaName.$sequenceName " + definition + " "
    val expected2  = s"CREATE SEQUENCE $schemaName.$sequenceName " + definition + " "

    val result1 = create_sequence_ddl(sequenceName, Some(schemaName), definition, checkExistence = true)
    val result2 = create_sequence_ddl(sequenceName, Some(schemaName), definition, checkExistence = false)

    result1.update.sql shouldBe expected1
    result2.update.sql shouldBe expected2
  }

  it should "produce the correct fragment when deleting a sequence" in {
    val expected1 = s"DROP SEQUENCE IF EXISTS $schemaName.$sequenceName "
    val expected2 = s"DROP SEQUENCE $schemaName.$sequenceName "

    val result1 = drop_sequence_ddl(sequenceName, Some(schemaName), checkExistence = true)
    val result2 = drop_sequence_ddl(sequenceName, Some(schemaName), checkExistence = false)

    result1.update.sql shouldBe expected1
    result2.update.sql shouldBe expected2
  }

  "GenericRepository DML" should "produce the correct fragment when selecting from a table" in {
    val columns    = List("field1", "field2")
    val definition = "WHERE (field1 = ?)"
    val expected   = s"SELECT ${columns.mkString(", ")} FROM $schemaName.$tableName " + definition + " "

    val result = select_query(tableName, Some(schemaName), columns, definition)

    result.update.sql shouldBe expected
  }

  it should "produce the correct fragment when inserting in a table" in {
    val columns  = List("field1", "field2")
    val expected = s"INSERT INTO $schemaName.$tableName (${columns.mkString(", ")}) VALUES (?, ?) "

    val result = insert_query(tableName, Some(schemaName), columns)

    result.update.sql shouldBe expected
  }

  it should "produce the correct fragment when updating a table" in {
    val columns    = List("field1", "field2")
    val definition = "WHERE (field1 = ?)"
    val expected   = s"UPDATE $schemaName.$tableName SET field1 = ? , field2 = ? " + definition + " "

    val result = update_query(tableName, Some(schemaName), columns, definition)

    result.update.sql shouldBe expected
  }

  it should "produce the correct fragment when deleting a table" in {
    val definition = "WHERE (field1 = ?)"
    val expected   = s"DELETE FROM $schemaName.$tableName " + definition + " "

    val result = delete_query(tableName, Some(schemaName), definition)

    result.update.sql shouldBe expected
  }
}
