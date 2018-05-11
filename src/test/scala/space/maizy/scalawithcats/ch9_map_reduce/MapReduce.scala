package space.maizy.scalawithcats.ch9_map_reduce

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.Monoid

object MapReduce {
  def foldMap[A, B](seq: Vector[A])(f: A => B)(implicit bMonoid: Monoid[B]): B =
    seq.foldLeft(bMonoid.empty) { (acc, i) =>
      bMonoid.combine(acc, f(i))
    }
}
