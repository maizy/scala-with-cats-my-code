package space.maizy.scalawithcats.ch11_case_crdt

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.Monoid
import cats.syntax.semigroup._
import cats.instances.map._

final case class GCounter2[A](counters: Map[String, A]) {

  def increment(machine: String, amount: A)(implicit monoid: Monoid[A]): GCounter2[A] =
    GCounter2(
      counters.updated(machine, counters.getOrElse(machine, monoid.empty) |+| amount)
    )

  def merge(that: GCounter2[A])(implicit bsl: BoundedSemiLattice[A]): GCounter2[A] = {
    GCounter2(
      this.counters |+| that.counters
    )
  }


  def total()(implicit monoid: Monoid[A]): A = Monoid[A].combineAll(counters.values)

}
