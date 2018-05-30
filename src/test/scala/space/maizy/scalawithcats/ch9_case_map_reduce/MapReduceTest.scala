package space.maizy.scalawithcats.ch9_case_map_reduce

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import space.maizy.scalawithcats.BaseSpec

class MapReduceTest extends BaseSpec {

  "MapReduce.foldMap" should "works" in {
    import cats.instances.int._
    import cats.instances.string._
    MapReduce.foldMap(Vector(1, 2, 3))(identity) shouldBe 6
    MapReduce.foldMap(Vector(1, 2, 3))(_.toString + "! ") shouldBe "1! 2! 3! "
    MapReduce.foldMap("Hello world!".toVector)(_.toString.toUpperCase) shouldBe "HELLO WORLD!"
  }

  "MapReduce.parallelFoldMap" should "works" in {
    import cats.instances.int._
    val res = MapReduce.parallelFoldMap((1 to 100).toVector)(identity)
    Await.result(res, 1.minute) shouldBe 5050
  }

  "MapReduce.parallelFoldMapWithCats" should "works" in {
    import cats.instances.int._
    val res = MapReduce.parallelFoldMapWithCats((1 to 100).toVector)(identity)
    Await.result(res, 1.minute) shouldBe 5050
  }

}
