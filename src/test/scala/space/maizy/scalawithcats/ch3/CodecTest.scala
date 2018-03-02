package space.maizy.scalawithcats.ch3

/**
  * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
  * See LICENSE.txt for details.
  */

import org.scalatest.{ FlatSpec, Matchers }

class CodecTest extends FlatSpec with Matchers {
  "Codec" should "works with fully specified codecs" in {
    import CodecOps._
    import CodecInstances._

    encode(5) shouldBe "5"
    encode(true) shouldBe "true"
    encode("t") shouldBe "t"

    decode[Int]("5") shouldBe 5
    decode[String]("5") shouldBe "5"
    decode[Boolean]("yes") shouldBe true
  }

  it should "works for codec builded from imap" in {
    import CodecOps._
    import CodecInstances._

    encode(5.3) shouldBe "5.3"
    decode[Double]("5.3") shouldBe 5.3 +- 0.1
  }

  it should "works for boxed types" in {
    import CodecOps._
    import CodecInstances._

    encode(Box(1.234)) shouldBe "1.234"
    val v: Box[Double] = decode[Box[Double]]("1.234")
    v.value shouldBe 1.234 +- 0.1
  }
}
