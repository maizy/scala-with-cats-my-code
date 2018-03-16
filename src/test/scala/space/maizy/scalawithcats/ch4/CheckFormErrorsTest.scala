package space.maizy.scalawithcats.ch4

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import space.maizy.scalawithcats.BaseSpec
import cats.syntax.either._
import space.maizy.scalawithcats.ch4.CheckFormErrors.{ FormData, ParsedFormData }

class CheckFormErrorsTest extends BaseSpec {

  "CheckFormErrors" should "works" in {
    CheckFormErrors.alwaysFailedCheck() shouldBe "Always Failed".asLeft[ParsedFormData]
    CheckFormErrors.alwaysSuccessfullCheck() shouldBe ParsedFormData("test@example.com").asRight[String]

    CheckFormErrors.check(FormData("some@example.com")) shouldBe ParsedFormData("some@example.com").asRight[String]
    CheckFormErrors.check(FormData("someexample.com")) shouldBe "Wrong email".asLeft[ParsedFormData]

    CheckFormErrors.checkAndCheckLength(FormData("a@a.c")) shouldBe "Too short login".asLeft[ParsedFormData]

    CheckFormErrors.checkAndRecover(FormData("someexample.com")) shouldBe
      ParsedFormData("boss@example.com").asRight[String]
  }
}
