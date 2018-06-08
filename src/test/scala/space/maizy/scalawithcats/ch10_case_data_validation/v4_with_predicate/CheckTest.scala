package space.maizy.scalawithcats.ch10_case_data_validation.v4_with_predicate

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.syntax.validated._
import space.maizy.scalawithcats.BaseSpec

class CheckTest extends BaseSpec {
  type ErrorType = String

  val intPredicate: Predicate[ErrorType, Int] = Pure { v =>
    if (v != 0) {
      v.validNel[ErrorType]
    } else {
      "value is zero".invalidNel[Int]
    }
  }

  val intCheck = Check(intPredicate)

  "Check" should "supports pure with predicate" in {
    intCheck(1) shouldBe 1.validNel[ErrorType]
    intCheck(0) shouldBe "value is zero".invalidNel[Int]
  }

  it should "supports map" in {
    val checkIntAndTransform = intCheck.map(i => s"yay! $i != 0")
    checkIntAndTransform(2) shouldBe "yay! 2 != 0".validNel[ErrorType]
    checkIntAndTransform(0) shouldBe "value is zero".invalidNel[String]
  }
}
