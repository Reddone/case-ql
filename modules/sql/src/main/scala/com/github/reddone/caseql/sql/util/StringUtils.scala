package com.github.reddone.caseql.sql.util

import scala.annotation.tailrec

object StringUtils {

  def camelToSnake(str: String): String = {
    @tailrec
    def go(acc: List[Char], rest: List[Char]): List[Char] = rest match {
      case Nil                                                        => acc
      case a :: b :: c :: tail if a.isUpper && b.isUpper && c.isLower => go(acc ++ List(a, '_', b, c), tail)
      case a :: b :: tail if a.isLower && b.isUpper                   => go(acc ++ List(a, '_', b), tail)
      case a :: tail                                                  => go(acc :+ a, tail)
    }

    go(Nil, str.toList).mkString.toLowerCase
  }

  def addPrefix(str: String, prefix: Option[String], sep: String = "."): String =
    prefix.map(_ + sep).getOrElse("") + str

  def addSuffix(str: String, suffix: Option[String], sep: String = "."): String =
    str + suffix.map(sep + _).getOrElse("")

  def shorten(s: String): String = toAlphabetOnly(s).split("_").map(word => word.take(1)).mkString

  def toAlphabetOnly(str: String): String = {
    val filtered = str.filter(c => c.isLetter && c <= 'z' || c == '_')
    if (filtered.isEmpty) "x" else filtered
  }
}
