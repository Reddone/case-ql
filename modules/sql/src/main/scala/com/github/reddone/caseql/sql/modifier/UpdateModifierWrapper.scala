package com.github.reddone.caseql.sql.modifier

trait UpdateModifierWrapper[T <: UpdateModifierWrapper[T]] { self: T with Product =>
}
