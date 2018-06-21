package space.maizy.scalawithcats.ch11_case_crdt

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

final case class GCounter(counters: Map[String, Int]) {

  def increment(machine: String, amount: Int): GCounter =
    GCounter(
      counters.updated(machine, counters.getOrElse(machine, 0) + amount)
    )

  def merge(that: GCounter): GCounter = {
    val allMachines = (counters.keys ++ that.counters.keys).toList.distinct

    GCounter(
      allMachines.map { machine =>
        machine -> Math.max(counters.getOrElse(machine, 0), that.counters.getOrElse(machine, 0))
      }.toMap
    )
  }


  def total: Int = counters.values.sum

}
