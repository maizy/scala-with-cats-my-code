package space.maizy.scalawithcats.ch10_case_data_validation.v4_with_predicate

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.data.NonEmptyList
import cats.data.Validated
import cats.syntax.validated._
import space.maizy.scalawithcats.BaseSpec

class PredicateTest extends BaseSpec {

  import space.maizy.scalawithcats.ch10_case_data_validation.v4_with_predicate.PredicateOps._

  type ErrorType = String

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
    odd.run(0) shouldBe "0 isn't odd".invalidNel[Int]
    odd.run(2) shouldBe "2 isn't odd".invalidNel[Int]
    odd.run(1) shouldBe 1.validNel[ErrorType]
    nonZero.run(2) shouldBe 2.validNel[ErrorType]
  }

  it should "supports run & and" in {
    val oddNonZero  = nonZero and odd
    oddNonZero.run(2) shouldBe "2 isn't odd".invalidNel[Int]
    oddNonZero.run(0) shouldBe NonEmptyList.of("value is zero", "0 isn't odd").invalid[Int]
  }

  it should "supports or" in {
    val zeroOrOdd  = zero or odd
    zeroOrOdd.run(-1) shouldBe (-1).validNel[ErrorType]
    zeroOrOdd.run(0) shouldBe 0.validNel[ErrorType]
    zeroOrOdd.run(-2) shouldBe NonEmptyList.of("-2 isn't zero", "-2 isn't odd").invalid[Int]
  }

  it should "supports lift" in {
    val liftedPredicate = Predicate.lift[ErrorType, Int]("value is negative", _ >= 0)
    liftedPredicate.run(-1) shouldBe "value is negative".invalidNel[Int]
    liftedPredicate.run(0) shouldBe 0.validNel[ErrorType]
  }
}
