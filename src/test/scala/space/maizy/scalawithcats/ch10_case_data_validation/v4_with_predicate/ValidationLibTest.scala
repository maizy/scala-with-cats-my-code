package space.maizy.scalawithcats.ch10_case_data_validation.v4_with_predicate

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import space.maizy.scalawithcats.BaseSpec

class ValidationLibTest extends BaseSpec {

  import ValidationLib._

  "ValidationLib" should "check email" in {
    emailCheck("no") shouldBe invalid("Value must contains @ with chars before & after it")
    emailCheck("info@example.com") shouldBe valid("info@example.com")
    emailCheck("info@example") shouldBe invalid("Must contain the character .")
    emailCheck("@example") shouldBe invalid("Must be longer than 0 characters", "Must contain the character .")
    emailCheck("info@") shouldBe invalid("Value must contains @ with chars before & after it")
    emailCheck("info@i@i") shouldBe invalid("Value must contains @ with chars before & after it")
  }

  it should "check username" in {
    userNameCheck("li") shouldBe invalid("Must be longer than 3 characters")
    userNameCheck("maizy") shouldBe valid("maizy")
    userNameCheck("d&5") shouldBe invalid("Must be longer than 3 characters", "Must be all alphanumeric characters")
  }

}
