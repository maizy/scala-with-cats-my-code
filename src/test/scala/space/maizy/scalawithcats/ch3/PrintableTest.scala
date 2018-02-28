package space.maizy.scalawithcats.ch3

/**
  * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
  * See LICENSE.txt for details.
  */

import org.scalatest.{ FlatSpec, Matchers }

class PrintableTest extends FlatSpec with Matchers {
  import PrintableInstances._
  import Printable._

  "Printable.contramap" should "works with exact instanses" in {
    format("test") shouldBe "\"test\""
    format(false) shouldBe "no"
  }

  it should "works with contramap builded instances" in {
    format(Box("Test")) shouldBe "\"Test\""
    format(Box(true)) shouldBe "yes"
  }
}
