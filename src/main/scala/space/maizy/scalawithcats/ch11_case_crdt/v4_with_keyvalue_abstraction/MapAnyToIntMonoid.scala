package space.maizy.scalawithcats.ch11_case_crdt.v4_with_keyvalue_abstraction

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.Monoid

object MapAnyToIntMonoid {
  def genMonoid[A]: Monoid[Map[A, Int]] = new Monoid[Map[A, Int]] {
    override def empty: Map[A, Int] = Map.empty[A, Int]

    override def combine(x: Map[A, Int], y: Map[A, Int]): Map[A, Int] = {
      val allKeys = (x.keys ++ y.keys).toList.distinct
      allKeys.map { key =>
        key -> Math.max(x.getOrElse(key, Int.MinValue), y.getOrElse(key, Int.MinValue))
      }.toMap
    }
  }
}
