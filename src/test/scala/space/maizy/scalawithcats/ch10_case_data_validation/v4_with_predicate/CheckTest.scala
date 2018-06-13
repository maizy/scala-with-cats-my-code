package space.maizy.scalawithcats.ch10_case_data_validation.v4_with_predicate

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.Semigroup
import cats.data.{ NonEmptyList, ValidatedNel }
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

  it should "supports flatMap" in {

    // there is no other convential way to implement this check
    val stringCheck = new Check[ErrorType, Int, String] {
      override def apply(in: Int)(implicit sg: Semigroup[NonEmptyList[ErrorType]]): ValidatedNel[ErrorType, String] = {
        val strValue = in.toString
        if (strValue.length > 1) {
          s"$strValue is right value".validNel[ErrorType]
        } else {
          "should be more than 1 char".invalidNel[String]
        }
      }
    }

    val checkIntAndString: Check[ErrorType, Int, String] = intCheck.flatMap { i: Int =>
      stringCheck
    }
    checkIntAndString(20) shouldBe "20 is right value".validNel[ErrorType]
    checkIntAndString(0) shouldBe "value is zero".invalidNel[String]
    checkIntAndString(0) shouldBe "value is zero".invalidNel[String]
  }
}
