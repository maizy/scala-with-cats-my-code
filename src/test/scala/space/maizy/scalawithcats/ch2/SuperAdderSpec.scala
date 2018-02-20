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

  "SuperAdder" should "works" in {
    import cats.syntax.option._
    import cats.instances.int._
    import cats.instances.option._
    SuperAdder.add(List(1, 2, 5)) shouldBe 8
    SuperAdder.add(List(1.some, 2.some, none[Int])) shouldBe 3.some
    SuperAdder.add(List(none[Int], none[Int])) shouldBe none[Int]
  }
}
