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

  it should "supports flatMap" in {

    val stringCheck = Check.apply[ErrorType, Int, String] { in =>
      val strValue = in.toString
      if (strValue.length > 1) {
        s"$strValue is right value".validNel[ErrorType]
      } else {
        "should be more than 1 char".invalidNel[String]
      }
    }

    val checkIntAndString: Check[ErrorType, Int, String] = intCheck.flatMap { i: Int =>
      stringCheck
    }
    checkIntAndString(20) shouldBe "20 is right value".validNel[ErrorType]
    checkIntAndString(0) shouldBe "value is zero".invalidNel[String]
  }

  it should "supports andThen" in {
    val stringPredicate: Predicate[ErrorType, String] = Pure { v =>
      if (v.length < 2) {
        v.validNel[ErrorType]
      } else {
        s"value '$v' length more than 2".invalidNel[String]
      }
    }

    val stringCheck = Check(stringPredicate)
    stringCheck("100") shouldBe "value '100' length more than 2".invalidNel[Int]
    stringCheck("2") shouldBe "2".validNel[ErrorType]

    val intAndThenStringCheck = intCheck.map(_.toString) andThen stringCheck
    val stringAndThenIntCheck = stringCheck.map(_.toInt) andThen intCheck

    intAndThenStringCheck(1) shouldBe "1".validNel[ErrorType]
    intAndThenStringCheck(100) shouldBe "value '100' length more than 2".invalidNel[String]
    intAndThenStringCheck(0) shouldBe "value is zero".invalidNel[String]

    stringAndThenIntCheck("1") shouldBe 1.validNel[ErrorType]
    stringAndThenIntCheck("0") shouldBe "value is zero".invalidNel[Int]
    stringAndThenIntCheck("100") shouldBe "value '100' length more than 2".invalidNel[Int]
  }
}
