package space.maizy.scalawithcats.сh6_applicative

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.data.NonEmptyList
import space.maizy.scalawithcats.BaseSpec

class FormValidatorTest extends BaseSpec {

  "FormValidator.getValue" should "works" in {
    import cats.syntax.either._
    import FormValidator._
    val formData: FormData = Map("name" -> "Some")
    getValue("name", formData) shouldBe "Some".asRight[FirstErrorOr[String]]
    getValue("other", formData) shouldBe NonEmptyList.one("other is required").asLeft[FirstErrorOr[String]]
  }
}
