package space.maizy.scalawithcats.ch2

/**
  * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
  * See LICENSE.txt for details.
  */

import cats.Monoid

object BooleanMonoid extends Monoid[Boolean] {
  override def empty: Boolean = true
  override def combine(x: Boolean, y: Boolean): Boolean = x && y
}
