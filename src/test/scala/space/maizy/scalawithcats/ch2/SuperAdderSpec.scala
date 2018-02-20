package space.maizy.scalawithcats.ch2

/**
  * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
  * See LICENSE.txt for details.
  */

import org.scalatest.{ FlatSpec, Matchers }

class SuperAdderSpec extends FlatSpec with Matchers {

  "IntAdder" should "works" in {
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


    SuperAdder.add2(List(1, 2, 5)) shouldBe 8
    SuperAdder.add2(List(1.some, 2.some, none[Int])) shouldBe 3.some
    SuperAdder.add2(List(none[Int], none[Int])) shouldBe none[Int]

    // could not find implicit value for evidence parameter of type cats.Monoid[Double]
    // evidance is added implicitly because of [A : Monoid] type
    //SuperAdder.add2(List(1.0, 2.0, 5.0)) shouldBe 8.0
  }
}
