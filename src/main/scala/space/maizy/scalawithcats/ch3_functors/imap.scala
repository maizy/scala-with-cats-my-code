package space.maizy.scalawithcats.ch3_functors

/**
  * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
  * See LICENSE.txt for details.
  */

import cats.Monoid
import cats.instances.string._
import cats.syntax.invariant._

object SymbolMonoid {
  implicit val symbolMonoid: Monoid[Symbol] =
    Monoid[String].imap(Symbol.apply)(_.name)
}
