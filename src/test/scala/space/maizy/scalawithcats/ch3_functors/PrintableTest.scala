package space.maizy.scalawithcats.ch3_functors

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import space.maizy.scalawithcats.BaseSpec

class PrintableTest extends BaseSpec {
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
