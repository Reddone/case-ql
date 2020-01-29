package com.github.reddone.caseql.sql.modifier

trait DeleteModifierWrapper[T <: DeleteModifierWrapper[T]] { self: T with Product =>
}
