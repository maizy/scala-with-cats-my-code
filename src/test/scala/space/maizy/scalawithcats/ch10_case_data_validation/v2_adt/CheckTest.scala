package space.maizy.scalawithcats.ch10_case_data_validation.v2_adt

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.data.NonEmptyList
import cats.syntax.either._
import space.maizy.scalawithcats.BaseSpec


class CheckTest extends BaseSpec {

  import space.maizy.scalawithcats.ch10_case_data_validation.v2_adt.CheckOps._

  type Errors = NonEmptyList[String]

  val odd: Check[Errors, Int] = Pure { v =>
    Either.cond(v % 2 == 1, v, NonEmptyList.one(s"$v isn't odd"))
  }

  val nonZero: Check[NonEmptyList[String], Int] = Pure {v =>
    Either.cond(v != 0, v, NonEmptyList.one("value is zero"))
  }

  "Check based on ADT" should "works" in {
    odd.run(0) shouldBe NonEmptyList.one("0 isn't odd").asLeft[Int]
    odd.run(2) shouldBe NonEmptyList.one("2 isn't odd").asLeft[Int]
    odd.run(1) shouldBe 1.asRight[Errors]
    nonZero.run(2) shouldBe 2.asRight[Errors]
  }

  "and & run for check based on ADT" should "works" in {
    val oddNonZero  = nonZero and odd
    oddNonZero.run(2) shouldBe NonEmptyList.one("2 isn't odd").asLeft[Int]
    oddNonZero.run(0) shouldBe NonEmptyList.of("value is zero", "0 isn't odd").asLeft[Int]
  }
}
