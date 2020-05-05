package com.github.reddone.caseql.sql

import doobie._
import doobie.implicits._
import Fragment._

object PgHelper {

  // SCHEMA

  def createSchema(schemaName: String): Update0 = {
    const(s"CREATE SCHEMA $schemaName").update
  }

  def dropSchema(schemaName: String): Update0 = {
    const(s"DROP SCHEMA $schemaName").update
  }

  def checkSchema(schemaName: String): Query0[String] = {
    sql"SELECT schema_name FROM information_schema.schemata WHERE schema_name = $schemaName".query[String]
  }

  // TABLE

  def createTable(schemaName: String, tableName: String): Update0 = {
    const(s"CREATE TABLE $schemaName.$tableName (id BIGINT PRIMARY KEY)").update
  }

  def dropTable(schemaName: String, tableName: String): Update0 = {
    const(s"DROP TABLE $schemaName.$tableName").update
  }

  def checkTable(schemaName: String, tableName: String): Query0[Option[String]] = {
    const(s"SELECT to_regclass('$schemaName.$tableName')").query[Option[String]]
  }

  // SEQUENCE

  def createSequence(schemaName: String, sequenceName: String): Update0 = {
    const(s"CREATE SEQUENCE $schemaName.$sequenceName").update
  }

  def dropSequence(schemaName: String, sequenceName: String): Update0 = {
    const(s"DROP SEQUENCE $schemaName.$sequenceName").update
  }

  def checkSequence(schemaName: String, sequenceName: String): Query0[Option[String]] = {
    const(s"SELECT to_regclass('$schemaName.$sequenceName')").query[Option[String]]
  }
}
