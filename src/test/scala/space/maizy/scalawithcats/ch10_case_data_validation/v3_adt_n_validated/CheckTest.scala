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

  type ErrorType = NonEmptyList[String]

  val odd: Check[ErrorType, Int] = Pure { v =>
    if (v % 2 == 1) {
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

  "Check based on ADT" should "works" in {
    odd.run(0) shouldBe NonEmptyList.one("0 isn't odd").invalid[Int]
    odd.run(2) shouldBe NonEmptyList.one("2 isn't odd").invalid[Int]
    odd.run(1) shouldBe 1.valid[ErrorType]
    nonZero.run(2) shouldBe 2.valid[ErrorType]
  }

  "and & run for check based on ADT" should "works" in {
    val oddNonZero  = nonZero and odd
    oddNonZero.run(2) shouldBe NonEmptyList.one("2 isn't odd").invalid[Int]
    oddNonZero.run(0) shouldBe NonEmptyList.of("value is zero", "0 isn't odd").invalid[Int]
  }
}
