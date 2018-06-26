package space.maizy.scalawithcats.ch11_case_crdt.v1_naive

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import space.maizy.scalawithcats.BaseSpec

class GCounterTest extends BaseSpec {

  behavior of "GCounter"

  it should "increment" in {
    val empty = GCounter(Map.empty)
    val withA = empty.increment("A", 10)
    withA shouldBe GCounter(Map("A" -> 10))
    val withB = withA.increment("B", 2)
    withB shouldBe GCounter(Map("A" -> 10, "B" -> 2))
    withB.increment("A", 1) shouldBe GCounter(Map("A" -> 11, "B" -> 2))
  }

  it should "count total" in {
    GCounter(Map("A" -> 11, "B" -> 2)).total shouldBe 13
  }

  it should "merge" in {
    val merged = GCounter(Map("A" -> 11, "B" -> 2)) merge GCounter(Map("A" -> 9, "C" -> 5))
    merged shouldBe GCounter(Map("A" -> 11, "B" -> 2, "C" -> 5))
  }

}
