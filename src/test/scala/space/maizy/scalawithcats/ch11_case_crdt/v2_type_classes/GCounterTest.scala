package space.maizy.scalawithcats.ch11_case_crdt.v2_type_classes

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import space.maizy.scalawithcats.BaseSpec
import space.maizy.scalawithcats.ch11_case_crdt.v2_typeclasses.GCounter

class GCounterTest extends BaseSpec {

  behavior of "GCounter with type classes"

  it should "increment" in {
    import cats.instances.int._
    val empty = GCounter[Int](Map.empty)
    val withA = empty.increment("A", 10)
    withA shouldBe GCounter(Map("A" -> 10))
    val withB = withA.increment("B", 2)
    withB shouldBe GCounter(Map("A" -> 10, "B" -> 2))
    withB.increment("A", 1) shouldBe GCounter(Map("A" -> 11, "B" -> 2))
  }

  it should "count total" in {
    import cats.instances.int._
    GCounter(Map("A" -> 11, "B" -> 2)).total shouldBe 13
  }

  it should "merge" in {
    val merged = GCounter(Map("A" -> 11, "B" -> 2)) merge GCounter(Map("A" -> 9, "C" -> 5))
    merged shouldBe GCounter(Map("A" -> 11, "B" -> 2, "C" -> 5))
  }

  it should "works with set" in {
    import cats.instances.set._

    val empty = GCounter[Set[String]](Map.empty)
    val withA = empty.increment("A", Set("x", "y"))
    withA shouldBe GCounter(Map("A" -> Set("x", "y")))

    val withB = withA.increment("B", Set("x", "b"))
    withB shouldBe GCounter(Map("A" -> Set("x", "y"), "B" -> Set("x", "b")))

    withB.increment("A", Set("z")) shouldBe
      GCounter(Map("A" -> Set("x", "y", "z"), "B" -> Set("x", "b")))

    withB.total shouldBe Set("b", "x", "y")

    (withB merge GCounter(Map("A" -> Set("w"), "C" -> Set("...", ",,,")))) shouldBe
      GCounter(Map(
        "A" -> Set("x", "y", "w"),
        "B" -> Set("x", "b"),
        "C" -> Set("...", ",,,")
      ))
  }

}
