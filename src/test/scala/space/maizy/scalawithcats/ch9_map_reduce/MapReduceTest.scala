package space.maizy.scalawithcats.ch9_map_reduce

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import space.maizy.scalawithcats.BaseSpec

class MapReduceTest extends BaseSpec {

  "MapReduce.foldMap" should "works" in {
    import cats.instances.int._
    import cats.instances.string._
    MapReduce.foldMap(Vector(1, 2, 3))(identity) shouldBe 6
    MapReduce.foldMap(Vector(1, 2, 3))(_.toString + "! ") shouldBe "1! 2! 3! "
    MapReduce.foldMap("Hello world!".toVector)(_.toString.toUpperCase) shouldBe "HELLO WORLD!"
  }

}
