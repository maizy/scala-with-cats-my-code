package space.maizy.scalawithcats.ch10_case_data_validation.v3_adt_n_validated

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.data.NonEmptyList
import cats.data.Validated
import cats.syntax.validated._
import space.maizy.scalawithcats.BaseSpec

class CheckTest extends BaseSpec {

  import space.maizy.scalawithcats.ch10_case_data_validation.v3_adt_n_validated.CheckOps._

    type ErrorType = String

  val odd: Check[ErrorType, Int] = Pure { v =>
    if (v.abs % 2 == 1) {
      Validated.valid(v)
    } else {
      Validated.invalidNel(s"$v isn't odd")
    }
  }

  val nonZero: Check[ErrorType, Int] = Pure { v =>
    if (v != 0) {
      Validated.valid(v)
    } else {
      Validated.invalidNel("value is zero")
    }
  }

  val zero: Check[ErrorType, Int] = Pure { v =>
    if (v == 0) {
      Validated.valid(v)
    } else {
      Validated.invalidNel(s"$v isn't zero")
    }
  }

  "Check based on ADT" should "works" in {
    odd.run(0) shouldBe "0 isn't odd".invalidNel[Int]
    odd.run(2) shouldBe "2 isn't odd".invalidNel[Int]
    odd.run(1) shouldBe 1.validNel[ErrorType]
    nonZero.run(2) shouldBe 2.validNel[ErrorType]
  }

  "Check based on ADT" should "supports run & and" in {
    val oddNonZero  = nonZero and odd
    oddNonZero.run(2) shouldBe "2 isn't odd".invalidNel[Int]
    oddNonZero.run(0) shouldBe NonEmptyList.of("value is zero", "0 isn't odd").invalid[Int]
  }

  "Check based on ADT" should "supports or" in {
    val zeroOrOdd  = zero or odd
    zeroOrOdd.run(-1) shouldBe (-1).validNel[ErrorType]
    zeroOrOdd.run(0) shouldBe 0.validNel[ErrorType]
    zeroOrOdd.run(-2) shouldBe NonEmptyList.of("-2 isn't zero", "-2 isn't odd").invalid[Int]
  }
}
