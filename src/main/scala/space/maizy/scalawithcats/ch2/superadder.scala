package space.maizy.scalawithcats.ch2

/**
  * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
  * See LICENSE.txt for details.
  */

import cats.Monoid
import cats.syntax.semigroup._

object IntAdder {
  def add(items: List[Int]): Int = items.foldLeft(0)(_ + _)
}

object SuperAdder {

  def add[A](items: List[A])(implicit m: Monoid[A]): A =
    items.foldLeft(m.empty)(_ |+| _)

  def add2[A : Monoid](items: List[A]): A =
    items.foldLeft(Monoid[A].empty)(_ |+| _)
}
