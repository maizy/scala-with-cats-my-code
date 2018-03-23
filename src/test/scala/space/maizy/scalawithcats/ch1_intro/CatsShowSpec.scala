package space.maizy.scalawithcats.ch1_intro

/**
  * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
  * See LICENSE.txt for details.
  */

import space.maizy.scalawithcats.BaseSpec

class CatsShowSpec extends BaseSpec {

  "ShapeShow" should "work" in {
    import ShapeShowInstances._
    import cats.syntax.show._
    new Shape(5.0).show shouldBe "Shape: volume=5.0"
    new Square(2.0).show shouldBe "Square: side=2.0, volume=4.0"

    new Circle(8.0).show shouldBe "Shape: volume=201.1"
  }
}
