package space.maizy.scalawithcats.ch11_case_crdt.v2_typeclasses

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.Monoid
import cats.instances.map._
import cats.syntax.semigroup._
import space.maizy.scalawithcats.ch11_case_crdt.BoundedSemiLattice

final case class GCounter[A](counters: Map[String, A]) {

  def increment(machine: String, amount: A)(implicit monoid: Monoid[A]): GCounter[A] =
    GCounter(
      counters.updated(machine, counters.getOrElse(machine, monoid.empty) |+| amount)
    )

  def merge(that: GCounter[A])(implicit bsl: BoundedSemiLattice[A]): GCounter[A] = {
    GCounter(
      this.counters |+| that.counters
    )
  }


  def total()(implicit monoid: Monoid[A]): A = Monoid[A].combineAll(counters.values)

}
