package space.maizy.scalawithcats.ch10_case_data_validation.v5_with_kleisli

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.data.{ NonEmptyList, Validated }
import cats.syntax.either._
import space.maizy.scalawithcats.BaseSpec

class PredicateTest extends BaseSpec {

  import PredicateOps._

  type ErrorType = String
  type ErrorsType = NonEmptyList[ErrorType]

  val odd: Predicate[ErrorType, Int] = Pure { v =>
    if (v.abs % 2 == 1) {
      Validated.valid(v)
    } else {
      Validated.invalidNel(s"$v isn't odd")
    }
  }

  val nonZero: Predicate[ErrorType, Int] = Pure { v =>
    if (v != 0) {
      Validated.valid(v)
    } else {
      Validated.invalidNel("value is zero")
    }
  }

  val zero: Predicate[ErrorType, Int] = Pure { v =>
    if (v == 0) {
      Validated.valid(v)
    } else {
      Validated.invalidNel(s"$v isn't zero")
    }
  }

  "Predicate based on ADT" should "works" in {
    odd.run(0) shouldBe NonEmptyList.one("0 isn't odd").asLeft[Int]
    odd.run(2) shouldBe NonEmptyList.one("2 isn't odd").asLeft[Int]
    odd.run(1) shouldBe 1.asRight[ErrorsType]
    nonZero.run(2) shouldBe 2.asRight[ErrorsType]
  }

  it should "supports run & and" in {
    val oddNonZero  = nonZero and odd
    oddNonZero.run(2) shouldBe NonEmptyList.one("2 isn't odd").asLeft[Int]
    oddNonZero.run(0) shouldBe NonEmptyList.of("value is zero", "0 isn't odd").asLeft[Int]
  }

  it should "supports or" in {
    val zeroOrOdd  = zero or odd
    zeroOrOdd.run(-1) shouldBe (-1).asRight[ErrorsType]
    zeroOrOdd.run(0) shouldBe 0.asRight[ErrorsType]
    zeroOrOdd.run(-2) shouldBe NonEmptyList.of("-2 isn't zero", "-2 isn't odd").asLeft[Int]
  }

  it should "supports lift" in {
    val liftedPredicate = Predicate.lift[ErrorType, Int]("value is negative", _ >= 0)
    liftedPredicate.run(-1) shouldBe NonEmptyList.one("value is negative").asLeft[Int]
    liftedPredicate.run(0) shouldBe 0.asRight[ErrorsType]
  }
}
