package space.maizy.scalawithcats.ch11_case_crdt

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import space.maizy.scalawithcats.BaseSpec

class GCounter2Test extends BaseSpec {

  behavior of "GCounter2"

  it should "increment" in {
    import cats.instances.int._
    val empty = GCounter2[Int](Map.empty)
    val withA = empty.increment("A", 10)
    withA shouldBe GCounter2(Map("A" -> 10))
    val withB = withA.increment("B", 2)
    withB shouldBe GCounter2(Map("A" -> 10, "B" -> 2))
    withB.increment("A", 1) shouldBe GCounter2(Map("A" -> 11, "B" -> 2))
  }

  it should "count total" in {
    import cats.instances.int._
    GCounter2(Map("A" -> 11, "B" -> 2)).total shouldBe 13
  }

  it should "merge" in {
    val merged = GCounter2(Map("A" -> 11, "B" -> 2)) merge GCounter2(Map("A" -> 9, "C" -> 5))
    merged shouldBe GCounter2(Map("A" -> 11, "B" -> 2, "C" -> 5))
  }

  it should "works with set" in {
    import cats.instances.set._

    val empty = GCounter2[Set[String]](Map.empty)
    val withA = empty.increment("A", Set("x", "y"))
    withA shouldBe GCounter2(Map("A" -> Set("x", "y")))

    val withB = withA.increment("B", Set("x", "b"))
    withB shouldBe GCounter2(Map("A" -> Set("x", "y"), "B" -> Set("x", "b")))

    withB.increment("A", Set("z")) shouldBe
      GCounter2(Map("A" -> Set("x", "y", "z"), "B" -> Set("x", "b")))

    withB.total shouldBe Set("b", "x", "y")

    (withB merge GCounter2(Map("A" -> Set("w"), "C" -> Set("...", ",,,")))) shouldBe
      GCounter2(Map(
        "A" -> Set("x", "y", "w"),
        "B" -> Set("x", "b"),
        "C" -> Set("...", ",,,")
      ))
  }

}
