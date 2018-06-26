package space.maizy.scalawithcats.ch11_case_crdt.v3_with_map_abstraction

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import space.maizy.scalawithcats.BaseSpec

class GCounterTest extends BaseSpec {
  "GCounter with abstraction over storage" should "increment" in {
    import cats.instances.int._
    import GCounterMapInstances._

    val counter = GCounter[Map, String, Int]

    val empty = Map.empty[String, Int]
    val withA = counter.increment(empty)("A", 10)
    withA shouldBe Map("A" -> 10)
    val withB = counter.increment(withA)("B", 2)
    withB shouldBe Map("A" -> 10, "B" -> 2)
    counter.increment(withB)("A", 1) shouldBe Map("A" -> 11, "B" -> 2)
  }

  it should "count total" in {
    import cats.instances.int._
    import GCounterMapInstances._

    val counter = GCounter[Map, String, Int]

    val map = Map("A" -> 11, "B" -> 2)
    counter.total(map) shouldBe 13
  }

  it should "merge" in {
    import GCounterMapInstances._

    val counter = GCounter[Map, String, Int]

    val merged = counter.merge(Map("A" -> 11, "B" -> 2), Map("A" -> 9, "C" -> 5))
    merged shouldBe Map("A" -> 11, "B" -> 2, "C" -> 5)
  }
}
