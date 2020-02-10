package com.github.reddone.caseql.sql.table

import java.util.concurrent.atomic.AtomicLong

import scala.collection.concurrent.TrieMap

object TableRegistrar {

  private val prefix = "x"

  private val counter = new AtomicLong(0L)

  private val aliasMap = {
    val map = new TrieMap[String, String]()
    map.put("Unit", s"${prefix}0")
    map
  }

  def aliasFor(tpeName: String): String =
    aliasMap.getOrElseUpdate(tpeName, s"${prefix}${counter.incrementAndGet().toString}")
}
