package space.maizy.scalawithcats.ch4

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import space.maizy.scalawithcats.BaseSpec
import scala.collection.immutable.{ Seq => ISeq }
import cats.syntax.either._

class EitherTest extends BaseSpec {

  "EitherTransfomation.transformList" should "works" in {
    EitherTransfomation.sqareNonNegativeInts(ISeq(1, 2, -1)) shouldBe
      ISeq(1.asRight[String], 4.asRight[String], "Unable to use -1: must be positive or zero".asLeft[Int])
  }

  "EitherTransfomation.divideInts" should "works" in {
    EitherTransfomation.divideIntsAndMulBy2(6, 3) shouldBe 4.asRight[String]
    EitherTransfomation.divideIntsAndMulBy2(4, 0) shouldBe "Division by zero".asLeft[Int]
  }

}
