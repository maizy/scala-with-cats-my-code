package space.maizy.scalawithcats.ch10_case_data_validation

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.data.NonEmptyList
import cats.syntax.either._
import space.maizy.scalawithcats.BaseSpec

class CheckFTest extends BaseSpec {

  type Errors = NonEmptyList[String]
  val odd: Check[Errors, Int] = CheckF { v =>
    Either.cond(v % 2 == 1, v, NonEmptyList.one(s"$v isn't odd"))
  }

  val nonZero: Check[NonEmptyList[String], Int] = CheckF {v =>
    Either.cond(v != 0, v, NonEmptyList.one("value is zero"))
  }

  "CheckF" should "works" in {
    odd(0) shouldBe NonEmptyList.one("0 isn't odd").asLeft[Int]
    odd(2) shouldBe NonEmptyList.one("2 isn't odd").asLeft[Int]
    odd(1) shouldBe 1.asRight[Errors]
    nonZero(2) shouldBe 2.asRight[Errors]
  }

  "CheckF.and" should "works" in {
    val oddNonZero  = nonZero and odd

    oddNonZero(2) shouldBe NonEmptyList.one("2 isn't odd").asLeft[Int]
    oddNonZero(0) shouldBe NonEmptyList.of("value is zero", "0 isn't odd").asLeft[Int]
  }
}
