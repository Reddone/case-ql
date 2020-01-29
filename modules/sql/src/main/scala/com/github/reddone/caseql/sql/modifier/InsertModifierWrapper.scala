package com.github.reddone.caseql.sql.modifier

trait InsertModifierWrapper[T <: InsertModifierWrapper[T]] { self: T with Product =>
}
