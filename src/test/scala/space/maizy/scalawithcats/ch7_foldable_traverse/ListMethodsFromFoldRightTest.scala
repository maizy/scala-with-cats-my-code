package space.maizy.scalawithcats.ch7_foldable_traverse

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import space.maizy.scalawithcats.BaseSpec

class ListMethodsFromFoldRightTest extends BaseSpec {
  import ListMethodsFromFoldRight._

  "frMap" should "works" in {
    List(1, 2, 3, 4).frMap(_.toString) shouldBe List("1", "2", "3", "4")
  }

  "frFlatMap" should "works" in {
    List(1, 2, 3, 4).frFlatmap(i => if (i % 2 == 0) List(i, i) else Nil) shouldBe List(2, 2, 4, 4)
  }

  "frFilter" should "works" in {
    List(-2, -3, 2, 3, 4).frFilter(_ > 0) shouldBe List(2, 3, 4)
  }

  "frSum" should "works for Numeric types" in {
    List(1, 2, 3, 4).frSum shouldBe 10
    List(1L, 2L, 3L, 4L).frSum shouldBe 10L
    List(1.0, 2.0, 3.0, 4.0).frSum shouldBe 10.0
  }
}
