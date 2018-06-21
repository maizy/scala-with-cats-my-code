package space.maizy.scalawithcats.ch11_case_crdt

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.Monoid
import cats.syntax.monoid._

final case class GCounter2[A](counters: Map[String, A]) {

  def increment(machine: String, amount: A)(implicit monoid: Monoid[A]): GCounter2[A] =
    GCounter2(
      counters.updated(machine, counters.getOrElse(machine, Monoid[A].empty) |+| amount)
    )

  def merge(that: GCounter2[A])(implicit bsl: BoundedSemiLattice[A]): GCounter2[A] = {
    val allMachines = (counters.keys ++ that.counters.keys).toList.distinct

    val zero = bsl.empty
    GCounter2(
      allMachines.map { machine =>
        machine -> bsl.combine(counters.getOrElse(machine, zero), that.counters.getOrElse(machine, zero))
      }.toMap
    )
  }


  def total()(implicit monoid: Monoid[A]): A = Monoid[A].combineAll(counters.values)

}
