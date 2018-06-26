package space.maizy.scalawithcats.ch11_case_crdt.v4_with_keyvalue_abstraction

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import space.maizy.scalawithcats.BaseSpec

class KeyValueStoreTest extends BaseSpec {

  behavior of "KeyValueStore"

  import KeyValueStoreMapInstances._
  val s: KeyValueStore[Map] = KeyValueStore[Map]

  it should "put" in {
    val map = Map("a" -> 1)
    s.put(map)("b", 10) shouldBe Map("a" -> 1, "b" -> 10)
  }

  it should "get" in {
    val map = Map("a" -> 1)
    s.get(map)("a") shouldBe Some(1)
    s.get(map)("c") shouldBe Option.empty[Int]
  }

  it should "getOrElse" in {
    val map = Map("a" -> 1)
    s.getOrElse(map)("a", 0) shouldBe 1
    s.getOrElse(map)("c", 0) shouldBe 0
  }

  it should "returns values" in {
    val map = Map("a" -> 1, "b" -> 2)
    s.values(map).sorted shouldBe List(1, 2)
  }

}
