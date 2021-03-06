package com.github.reddone.caseql.sql.util

import cats.implicits._
import com.github.reddone.caseql.sql.functions
import com.github.reddone.caseql.sql.tokens.{Update => UpdateToken, _}
import doobie._
import Fragment._
import fs2.Stream

object GenericDDL {

  def create_schema_ddl(schemaName: String, checkExistence: Boolean): Fragment = {
    val sqlString = s"$Create $Schema${if (checkExistence) " " + IfNotExists else ""} $schemaName"
    const(sqlString)
  }

  def drop_schema_ddl(schemaName: String, checkExistence: Boolean): Fragment = {
    val sqlString = s"$Drop $Schema${if (checkExistence) " " + IfExists else ""} $schemaName"
    const(sqlString)
  }

  def create_table_ddl(
      tableName: String,
      schemaName: Option[String],
      tableDefinition: String,
      checkExistence: Boolean
  ): Fragment = {
    val sqlString = s"$Create $Table${if (checkExistence) " " + IfNotExists else ""} " +
      s"${StringUtils.addPrefix(tableName, schemaName)}"
    const(sqlString) ++ Fragments.parentheses(const(s"\n$tableDefinition"))
  }

  def drop_table_ddl(tableName: String, schemaName: Option[String], checkExistence: Boolean): Fragment = {
    val sqlString = s"$Drop $Table${if (checkExistence) " " + IfExists else ""} " +
      s"${StringUtils.addPrefix(tableName, schemaName)}"
    const(sqlString)
  }

  def create_sequence_ddl(
      sequenceName: String,
      schemaName: Option[String],
      sequenceDefinition: String,
      checkExistence: Boolean
  ): Fragment = {
    val sqlString = s"$Create $Sequence${if (checkExistence) " " + IfNotExists else ""} " +
      s"${StringUtils.addPrefix(sequenceName, schemaName)}"
    const(sqlString) ++ const(sequenceDefinition)
  }

  def drop_sequence_ddl(sequenceName: String, schemaName: Option[String], checkExistence: Boolean): Fragment = {
    val sqlString = s"$Drop $Sequence${if (checkExistence) " " + IfExists else ""} " +
      s"${StringUtils.addPrefix(sequenceName, schemaName)}"
    const(sqlString)
  }
}

object GenericDML {

  def select_query(
      tableName: String,
      schemaName: Option[String],
      columnNames: List[String],
      whereStatementDefinition: String
  ): Fragment = {
    val sqlString = s"$Select ${columnNames.mkString(", ")} $From ${StringUtils.addPrefix(tableName, schemaName)}"
    const(sqlString) ++ const(whereStatementDefinition)
  }

  def insert_query(tableName: String, schemaName: Option[String], columnNames: List[String]): Fragment = {
    val sqlString =
      s"$InsertInto ${StringUtils.addPrefix(tableName, schemaName)} (${columnNames.mkString(", ")}) $Values"
    const(sqlString) ++ functions.placeholders(columnNames.length)
  }

  def update_query(
      tableName: String,
      schemaName: Option[String],
      columnNames: List[String],
      whereStatementDefinition: String
  ): Fragment = {
    val sqlString = s"$UpdateToken ${StringUtils.addPrefix(tableName, schemaName)}"
    const(sqlString) ++
      Fragments.set(columnNames.map(col => const(s"$col = $Placeholder")): _*) ++
      const(whereStatementDefinition)
  }

  def delete_query(tableName: String, schemaName: Option[String], whereStatementDefinition: String): Fragment = {
    val sqlString = s"$Delete $From ${StringUtils.addPrefix(tableName, schemaName)}"
    const(sqlString) ++ const(whereStatementDefinition)
  }
}

object GenericRepository {

  def forSchema(schema: String): GenericRepository = new GenericRepository {
    import GenericDDL._
    import GenericDML._

    // #########################
    // ########## DDL ##########
    // #########################

    // SCHEMA

    override def createSchema(checkExistence: Boolean): ConnectionIO[Int] = {
      create_schema_ddl(schema, checkExistence).update.run
    }

    override def dropSchema(checkExistence: Boolean): ConnectionIO[Int] = {
      drop_schema_ddl(schema, checkExistence).update.run
    }

    // TABLE

    override def createTable(table: String, definition: String, checkExistence: Boolean): ConnectionIO[Int] = {
      create_table_ddl(table, Some(schema), definition, checkExistence).update.run
    }

    override def dropTable(table: String, checkExistence: Boolean): ConnectionIO[Int] = {
      drop_table_ddl(table, Some(schema), checkExistence).update.run
    }

    // SEQUENCE

    override def createSequence(sequence: String, definition: String, checkExistence: Boolean): ConnectionIO[Int] = {
      create_sequence_ddl(sequence, Some(schema), definition, checkExistence).update.run
    }

    override def dropSequence(sequence: String, checkExistence: Boolean): ConnectionIO[Int] = {
      drop_sequence_ddl(sequence, Some(schema), checkExistence).update.run
    }

    // #########################
    // ########## DML ##########
    // #########################

    // SELECT

    override def select[W: Write, R: Read](
        table: String,
        columns: List[String],
        whereStatement: String,
        whereParameters: W
    ): ConnectionIO[List[R]] = {
      val selectFragment = select_query(table, Some(schema), columns, whereStatement)
      FragmentUtils
        .wrapInQuery[W, R](selectFragment)
        .to[List](whereParameters)
    }

    override def selectStream[W: Write, R: Read](
        table: String,
        columns: List[String],
        whereStatement: String,
        whereParameters: W
    ): Stream[ConnectionIO, R] = {
      val selectFragment = select_query(table, Some(schema), columns, whereStatement)
      FragmentUtils
        .wrapInQuery[W, R](selectFragment)
        .stream(whereParameters)
    }

    // INSERT

    override def insert[W: Write](table: String, columns: List[String], valueParameters: W): ConnectionIO[Int] = {
      val insertFragment = insert_query(table, Some(schema), columns)
      FragmentUtils
        .wrapInUpdate[W](insertFragment)
        .run(valueParameters)
    }

    override def insertReturningKey[W: Write, R: Read](
        table: String,
        columns: List[String],
        valueParameters: W,
        returningColumns: List[String]
    ): ConnectionIO[R] = {
      val insertFragment = insert_query(table, Some(schema), columns)
      FragmentUtils
        .wrapInUpdate[W](insertFragment)
        .withUniqueGeneratedKeys[R](returningColumns: _*)(valueParameters)
    }

    override def insertMany[W: Write](
        table: String,
        columns: List[String],
        valueParametersList: List[W]
    ): ConnectionIO[Int] = {
      val insertFragment = insert_query(table, Some(schema), columns)
      FragmentUtils
        .wrapInUpdate[W](insertFragment)
        .updateMany(valueParametersList)
    }

    override def insertManyReturningKeys[W: Write, R: Read](
        table: String,
        columns: List[String],
        valueParametersList: List[W],
        returningColumns: List[String]
    ): Stream[ConnectionIO, R] = {
      val insertFragment = insert_query(table, Some(schema), columns)
      FragmentUtils
        .wrapInUpdate[W](insertFragment)
        .updateManyWithGeneratedKeys[R](returningColumns: _*)(valueParametersList)
    }

    // UPDATE

    override def update[W: Write](
        table: String,
        columns: List[String],
        whereStatement: String,
        valueAndWhereParameters: W
    ): ConnectionIO[Int] = {
      val updateFragment = update_query(table, Some(schema), columns, whereStatement)
      FragmentUtils
        .wrapInUpdate[W](updateFragment)
        .run(valueAndWhereParameters)
    }

    override def updateReturningKeys[W: Write, R: Read](
        table: String,
        columns: List[String],
        whereStatement: String,
        valueAndWhereParameters: W,
        returningColumns: List[String]
    ): Stream[ConnectionIO, R] = {
      val updateFragment = update_query(table, Some(schema), columns, whereStatement)
      FragmentUtils
        .wrapInUpdate[W](updateFragment)
        .withGeneratedKeys(returningColumns: _*)(valueAndWhereParameters)
    }

    override def updateMany[W: Write](
        table: String,
        columns: List[String],
        whereStatement: String,
        valueAndWhereParametersList: List[W]
    ): ConnectionIO[Int] = {
      val updateFragment = update_query(table, Some(schema), columns, whereStatement)
      FragmentUtils
        .wrapInUpdate[W](updateFragment)
        .updateMany(valueAndWhereParametersList)
    }

    override def updateManyReturningKeys[W: Write, R: Read](
        table: String,
        columns: List[String],
        whereStatement: String,
        valueAndWhereParametersList: List[W],
        returningColumns: List[String]
    ): Stream[ConnectionIO, R] = {
      val updateFragment = update_query(table, Some(schema), columns, whereStatement)
      FragmentUtils
        .wrapInUpdate[W](updateFragment)
        .updateManyWithGeneratedKeys(returningColumns: _*)(valueAndWhereParametersList)
    }

    // DELETE

    override def delete[W: Write](table: String, whereStatement: String, whereParameters: W): ConnectionIO[Int] = {
      val deleteFragment = delete_query(table, Some(schema), whereStatement)
      FragmentUtils
        .wrapInUpdate[W](deleteFragment)
        .toUpdate0(whereParameters)
        .run
    }

    override def deleteReturningKeys[W: Write, R: Read](
        table: String,
        whereStatement: String,
        whereParameters: W,
        returningColumns: List[String]
    ): Stream[ConnectionIO, R] = {
      val deleteFragment = delete_query(table, Some(schema), whereStatement)
      FragmentUtils
        .wrapInUpdate[W](deleteFragment)
        .toUpdate0(whereParameters)
        .withGeneratedKeys[R](returningColumns: _*)
    }

    override def deleteMany[W: Write](
        table: String,
        whereStatement: String,
        whereParametersList: List[W]
    ): ConnectionIO[Int] = {
      val deleteFragment = delete_query(table, Some(schema), whereStatement)
      FragmentUtils
        .wrapInUpdate[W](deleteFragment)
        .updateMany(whereParametersList)
    }

    override def deleteManyReturningKeys[W: Write, R: Read](
        table: String,
        whereStatement: String,
        whereParametersList: List[W],
        returningColumns: List[String]
    ): Stream[ConnectionIO, R] = {
      val deleteFragment = delete_query(table, Some(schema), whereStatement)
      FragmentUtils
        .wrapInUpdate[W](deleteFragment)
        .updateManyWithGeneratedKeys[R](returningColumns: _*)(whereParametersList)
    }
  }
}

trait GenericRepository {

  // #########################
  // ########## DDL ##########
  // #########################

  // SCHEMA

  def createSchema(checkExistence: Boolean = true): ConnectionIO[Int]

  def dropSchema(checkExistence: Boolean = true): ConnectionIO[Int]

  // TABLE

  def createTable(table: String, definition: String, checkExistence: Boolean = true): ConnectionIO[Int]

  def dropTable(table: String, checkExistence: Boolean = true): ConnectionIO[Int]

  // SEQUENCE

  def createSequence(sequence: String, definition: String, checkExistence: Boolean = true): ConnectionIO[Int]

  def dropSequence(sequence: String, checkExistence: Boolean = true): ConnectionIO[Int]

  // #########################
  // ########## DML ##########
  // #########################

  // SELECT

  def select[W: Write, R: Read](
      table: String,
      columns: List[String],
      whereStatement: String,
      whereParameters: W
  ): ConnectionIO[List[R]]

  def selectStream[W: Write, R: Read](
      table: String,
      columns: List[String],
      whereStatement: String,
      whereParameters: W
  ): Stream[ConnectionIO, R]

  // INSERT

  def insert[W: Write](table: String, columns: List[String], valueParameters: W): ConnectionIO[Int]

  def insertReturningKey[W: Write, R: Read](
      table: String,
      columns: List[String],
      valueParameters: W,
      returningColumns: List[String]
  ): ConnectionIO[R]

  def insertMany[W: Write](table: String, columns: List[String], valueParametersList: List[W]): ConnectionIO[Int]

  def insertManyReturningKeys[W: Write, R: Read](
      table: String,
      columns: List[String],
      valueParametersList: List[W],
      returningColumns: List[String]
  ): Stream[ConnectionIO, R]

  // UPDATE

  def update[W: Write](
      table: String,
      columns: List[String],
      whereStatement: String,
      valueAndWhereParameters: W
  ): ConnectionIO[Int]

  def updateReturningKeys[W: Write, R: Read](
      table: String,
      columns: List[String],
      whereStatement: String,
      valueAndWhereParameters: W,
      returningColumns: List[String]
  ): Stream[ConnectionIO, R]

  def updateMany[W: Write](
      table: String,
      columns: List[String],
      whereStatement: String,
      valueAndWhereParametersList: List[W]
  ): ConnectionIO[Int]

  def updateManyReturningKeys[W: Write, R: Read](
      table: String,
      columns: List[String],
      whereStatement: String,
      valueAndWhereParametersList: List[W],
      returningColumns: List[String]
  ): Stream[ConnectionIO, R]

  // DELETE

  def delete[W: Write](table: String, whereStatement: String, whereParameters: W): ConnectionIO[Int]

  def deleteReturningKeys[W: Write, R: Read](
      table: String,
      whereStatement: String,
      whereParameters: W,
      returningColumns: List[String]
  ): Stream[ConnectionIO, R]

  def deleteMany[W: Write](table: String, whereStatement: String, whereParametersList: List[W]): ConnectionIO[Int]

  def deleteManyReturningKeys[W: Write, R: Read](
      table: String,
      whereStatement: String,
      whereParametersList: List[W],
      returningColumns: List[String]
  ): Stream[ConnectionIO, R]
}
