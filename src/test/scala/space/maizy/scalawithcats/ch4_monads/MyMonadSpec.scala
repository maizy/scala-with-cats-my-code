package space.maizy.scalawithcats.ch4_monads

import space.maizy.scalawithcats.BaseSpec

/**
  * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
  * See LICENSE.txt for details.
  */
class MyMonadSpec extends BaseSpec {
  "MyMonoid" should "implements map based on flatmap & pure" in {
    import MyMonadInstances._

    val value: Box[String] = Box("1234")

    boxMonad.flatMap(value)(v => Box(v.toInt)) shouldBe Box(1234)
    boxMonad.map(value)(_.toInt) shouldBe Box(1234)
  }

  "MyMonoid.MyId" should "works" in {
    import MyMonadInstances._
    val v: MyId[Int] = 5
    idMonad.flatMap(v)(_ + 2)  shouldBe 7
    idMonad.map(v)(_ + 2)  shouldBe 7

  }
}
