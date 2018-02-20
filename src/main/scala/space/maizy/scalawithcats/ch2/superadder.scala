package space.maizy.scalawithcats.ch2

/**
  * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
  * See LICENSE.txt for details.
  */

import cats.Monoid

object IntAdder {
  def add(items: List[Int])(implicit m: Monoid[Int]): Int = {
    items.foldLeft(m.empty)(m.combine)
  }
}

object SuperAdder {
  def add[A](items: List[A])(implicit m: Monoid[A]): A = {
    items.foldLeft(m.empty)(m.combine)
  }
}
