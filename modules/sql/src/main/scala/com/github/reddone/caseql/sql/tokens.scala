package com.github.reddone.caseql.sql

// Inspired to: http://ns.inria.fr/ast/sql/index.html
object tokens
    extends CommonTokens
    with ExpressionTokens
    with OperatorTokens
    with FunctionTokens
    with StatementTokens
    with CatalogObjectTokens

trait CommonTokens {

  // common tokens

  final val On = "ON"

  final val Default = "DEFAULT"

  final val True = "TRUE"

  final val False = "FALSE"

  final val Null = "NULL"

  final val Placeholder = "?"

  final val IfExists = "IF EXISTS"

  final val IfNotExists = "IF NOT EXISTS"
}

trait ExpressionTokens { self: CommonTokens =>

  // exist expression tokens

  final val Exists = "EXISTS"

  final val NotExists = "NOT EXISTS"

  final val Any = "ANY"

  final val All = "ALL"

  // join expression tokens

  final val CrossJoin = "CROSS JOIN"

  final val InnerJoin = "INNER JOIN"

  final val LeftJoin = "LEFT OUTER JOIN"

  final val RightJoin = "RIGHT OUTER JOIN"

  final val FullJoin = "FULL OUTER JOIN"

  // like expression tokens

  final val Like = "LIKE"

  final val NotLike = "NOT LIKE"

  // range expression tokens

  final val Between = "BETWEEN"

  final val NotBetween = "NOT BETWEEN"

  // null expression tokens

  final val IsNull = "IS NULL"

  final val IsNotNull = "IS NOT NULL"

  // case expression tokens

  final val When = "WHEN"

  final val Then = "THEN"

  final val Case = "CASE"

  final val Else = "ELSE"

  final val End = "END"

  // cte expression tokens

  final val With = "WITH"

  // window expression tokens

  final val Over = "OVER"
}

trait OperatorTokens { self: CommonTokens =>

  // wildcard operator tokens

  final val Star = "*"

  // alias operator tokens

  final val As = "AS"

  // arithmetic operator tokens

  final val Plus = "+"

  final val Minus = "-"

  final val MultipliedBy = "*"

  final val DividedBy = "/"

  final val Modulo = "%"

  // comparison operator tokens

  final val Equal = "="

  final val NotEqual = "<>"

  final val LessThan = "<"

  final val LessThanEqual = "<="

  final val GreaterThan = ">"

  final val GreaterThanEqual = ">="

  // logical operator tokens

  final val And = "AND"

  final val Or = "OR"

  final val Not = "NOT"

  // set operator tokens

  final val Intersect = "INTERSECT"

  final val IntersectAll = "INTERSECT ALL"

  final val Union = "UNION"

  final val UnionAll = "UNION ALL"

  final val Except = "EXCEPT"

  final val ExceptAll = "EXCEPT ALL"

  // string operator tokens

  final val ConcatWith = "||"
}

trait FunctionTokens { self: CommonTokens =>

  // aggregate function tokens

  final val Sum = "SUM"

  final val Count = "COUNT"

  final val Min = "MIN"

  final val Max = "MAX"

  final val Avg = "AVG"

  // number function tokens

  // string function tokens

  final val Ascii = "ASCII"

  final val Concat = "CONCAT"

  final val Upper = "UPPER"

  final val Lower = "LOWER"

  // date time function tokens

  final val Now = "NOW"

  // data type conversion function tokens

  final val Cast = "CAST"
}

trait StatementTokens { self: CommonTokens =>

  // access control statement tokens

  final val Grant = "GRANT"

  final val Revoke = "REVOKE"

  // data definition statement tokens

  final val Create = "CREATE"

  final val Alter = "ALTER"

  final val Drop = "DROP"

  // data manipulation statement tokens

  final val Select = "SELECT"

  final val Distinct = "DISTINCT"

  final val From = "FROM"

  final val Where = "WHERE"

  final val OrderBy = "ORDER BY"

  final val Asc = "ASC"

  final val Desc = "DESC"

  final val NullsFirst = "NULLS FIRST"

  final val NullsLast = "NULLS LAST"

  final val GroupBy = "GROUP BY"

  final val Having = "HAVING"

  final val InsertInto = "INSERT INTO"

  final val Values = "VALUES"

  final val Update = "UPDATE"

  final val Delete = "DELETE"

  final val PartitionBy = "PARTITION BY"

  // procedural statement tokens

  final val Call = "CALL"
}

trait CatalogObjectTokens { self: CommonTokens =>

  // catalog object tokens

  final val Schema = "SCHEMA"

  final val Table = "TABLE"

  final val Procedure = "PROCEDURE"

  final val Column = "COLUMN"

  final val Role = "ROLE"

  final val View = "VIEW"

  final val User = "USER"

  final val Sequence = "SEQUENCE"

  final val Index = "INDEX"
}
