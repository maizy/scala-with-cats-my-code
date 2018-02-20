package space.maizy.scalawithcats.ch2

/**
  * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
  * See LICENSE.txt for details.
  */

import org.scalatest.{ FlatSpec, Matchers }

class SuperAdderSpec extends FlatSpec with Matchers {

  "IntAdder" should "works" in {
    import cats.instances.int._
    IntAdder.add(List(1, 2, 5)) shouldBe 8
    IntAdder.add(List.empty) shouldBe 0
  }
}
