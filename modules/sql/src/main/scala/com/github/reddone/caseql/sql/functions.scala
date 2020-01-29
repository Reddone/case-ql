package com.github.reddone.caseql.sql

import cats.implicits._
import cats.Show
import com.github.reddone.caseql.sql.util.StringUtils
import doobie._
import doobie.implicits._
import Fragment._
import tokens._

// Inspired to: http://ns.inria.fr/ast/sql/index.html
object functions
    extends CommonFunctions
    with ExpressionFunctions
    with OperatorFunctions
    with FunctionFunctions
    with StatementFunctions
    with CatalogObjectFunctions

trait CommonFunctions {

  // common functions

  final def placeholders(n: Int): Fragment =
    const(s"(${List.fill(n)(Placeholder).mkString(", ")})")

  final def $(name: String, prefix: Option[String] = None): Fragment =
    const(StringUtils.addPrefix(name, prefix))

  final def $$(names: List[String], prefix: Option[String] = None): Fragment =
    const(names.map(StringUtils.addPrefix(_, prefix)).mkString(", "))
}

trait ExpressionFunctions { self: CommonFunctions =>

  // exist expression functions

  final def exists(subQuery: Fragment): Fragment =
    const(Exists) ++ Fragments.parentheses(subQuery)

  final def notExists(subQuery: Fragment): Fragment =
    const(NotExists) ++ Fragments.parentheses(subQuery)

  final def any(subQuery: Fragment): Fragment =
    const(Any) ++ Fragments.parentheses(subQuery)

  final def all(subQuery: Fragment): Fragment =
    const(All) ++ Fragments.parentheses(subQuery)

  // join expression functions

  // like expression functions

  final def like[T: Show](value: T): Fragment =
    const(Like) ++ fr"${Show[T].show(value)}"

  final def like(expr: Fragment): Fragment =
    const(Like) ++ expr

  final def notLike[T: Show](value: T): Fragment =
    const(NotLike) ++ fr"${Show[T].show(value)}"

  final def notLike(expr: Fragment): Fragment =
    const(NotLike) ++ expr

  // range expression functions

  final def between[A: Put, B: Put](leftValue: A, rightValue: B): Fragment =
    const(Between) ++ fr"$leftValue" ++ const(And) ++ fr"$rightValue"

  final def between(leftExpr: Fragment, rightExpr: Fragment): Fragment =
    const(Between) ++ leftExpr ++ const(And) ++ rightExpr

  final def notBetween[A: Put, B: Put](leftValue: A, rightValue: B): Fragment =
    const(NotBetween) ++ fr"$leftValue" ++ const(And) ++ fr"$rightValue"

  final def notBetween(leftExpr: Fragment, rightExpr: Fragment): Fragment =
    const(NotBetween) ++ leftExpr ++ const(And) ++ rightExpr

  // null expression functions

  // case expression functions

  final def whenThen[T: Put](thenValue: T, whenExpr: Fragment): Fragment =
    const(When) ++ whenExpr ++ const(Then) ++ fr"$thenValue"

  final def whenThen(thenExpr: Fragment, whenExpr: Fragment): Fragment =
    const(When) ++ whenExpr ++ const(Then) ++ thenExpr

  final def caseElse[T: Put](elseValue: T, caseExprs: Fragment*): Fragment =
    const(Case) ++
      caseExprs.toList.combineAll ++
      const(Else) ++ fr"$elseValue" ++
      const(End)

  final def caseElse(elseExpr: Fragment, caseExprs: Fragment*): Fragment =
    const(Case) ++
      caseExprs.toList.combineAll ++
      const(Else) ++ elseExpr ++
      const(End)

  // cte expression functions

  final def withAs(withExpr: Fragment, asSubQuery: Fragment): Fragment =
    const(With) ++ withExpr ++ const(As) ++ Fragments.parentheses(asSubQuery)

  // window expression functions

  final def over(expr: Fragment): Fragment =
    const(Over) ++ Fragments.parentheses(expr)
}

trait OperatorFunctions { self: CommonFunctions =>

  // wildcard operator functions

  // alias operator functions

  final def as(name: String): Fragment =
    const(s"AS $name")

  // arithmetic operator functions

  // comparison operator functions

  // logical operator functions

  // set operator functions

  // string operator functions
}

trait FunctionFunctions { self: CommonFunctions =>

  // aggregate function functions

  final def sum(name: String): Fragment =
    const(Sum + s" ($name)")

  final def sum(expr: Fragment): Fragment =
    const(Sum) ++ Fragments.parentheses(expr)

  final def count(name: String): Fragment =
    const(Count + s" ($name)")

  final def count(expr: Fragment): Fragment =
    const(Count) ++ Fragments.parentheses(expr)

  final def min(name: String): Fragment =
    const(Min + s" ($name)")

  final def min(expr: Fragment): Fragment =
    const(Min) ++ Fragments.parentheses(expr)

  final def max(name: String): Fragment =
    const(Max + s" ($name)")

  final def max(expr: Fragment): Fragment =
    const(Max) ++ Fragments.parentheses(expr)

  final def avg(name: String): Fragment =
    const(Avg + s" ($name)")

  final def avg(expr: Fragment): Fragment =
    const(Avg) ++ Fragments.parentheses(expr)

  // number function functions

  // string function functions

  final def ascii(value: String): Fragment =
    const(Ascii + s" ($value)")

  final def ascii(expr: Fragment): Fragment =
    const(Ascii) ++ Fragments.parentheses(expr)

  final def concat(value: String, values: String*): Fragment =
    const(Concat + s" ($value, ${values.toList.mkString(", ")})")

  final def concat(expr: Fragment, exprs: Fragment*): Fragment =
    const(Concat) ++ Fragments.parentheses(expr ++ const(",") ++ exprs.toList.intercalate(const(",")))

  final def upperCase(value: String): Fragment =
    const(Upper + s" ($value)")

  final def upperCase(expr: Fragment): Fragment =
    const(Upper) ++ Fragments.parentheses(expr)

  final def lowerCase(value: String): Fragment =
    const(Lower + s" ($value)")

  final def lowerCase(expr: Fragment): Fragment =
    const(Lower) ++ Fragments.parentheses(expr)

  // date time function functions

  final def now: Fragment =
    const(Now + "()")

  // data type conversion function functions

  final def cast(expr: Fragment, dataType: String): Fragment =
    const(Cast) ++ Fragments.parentheses(expr ++ const(As + " " + dataType))
}

trait StatementFunctions { self: CommonFunctions =>

  // access control statement functions

  // data definition statement functions

  // data manipulation statement functions

  // procedural statement functions
}

trait CatalogObjectFunctions { self: CommonFunctions =>

  // catalog object functions
}
