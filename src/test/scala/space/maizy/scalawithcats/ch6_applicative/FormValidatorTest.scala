package space.maizy.scalawithcats.ch6_applicative

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.data.NonEmptyList
import space.maizy.scalawithcats.BaseSpec

class FormValidatorTest extends BaseSpec {

  import cats.syntax.either._
  import cats.syntax.validated._
  import FormValidator._

  "FormValidator.getValue" should "works" in {
    val formData: FormData = Map("name" -> "Some")
    getValue("name")(formData) shouldBe "Some".asRight[ErrorsList]
    getValue("other")(formData) shouldBe NonEmptyList.one("other is required").asLeft[String]
  }

  "FormValidator.parseInt" should "works" in {
    parseInt("age")("12") shouldBe 12.asRight[ErrorsList]
    parseInt("age")("abc") shouldBe
      NonEmptyList.one("Unable to convert age ('abc') to int: NumberFormatException")
        .asLeft[String]
  }

  "FormValidator.nonBlank" should "works" in {
    nonBlank("age")("12 ") shouldBe "12".asRight[ErrorsList]
    nonBlank("age")(" ") shouldBe NonEmptyList.one("Field age is empty").asLeft[String]
  }

  "FormValidator.readName" should "works" in {
    readName(Map("name" -> "Some ")) shouldBe "Some".asRight[ErrorsList]
    readName(Map("name" -> "")) shouldBe NonEmptyList.one("Field name is empty").asLeft[String]
  }

  "FormValidator.readAge" should "works" in {
    readAge(Map("age" -> "100")) shouldBe 100.asRight[ErrorsList]
    readAge(Map("age" -> "abc")) shouldBe
      NonEmptyList.one("Unable to convert age ('abc') to int: NumberFormatException").asLeft[String]
  }

  "FormValidator.getUser" should "works" in {
    FormValidator.getUser(Map("age" -> "100", "name" -> "Buddy ")) shouldBe User("Buddy", 100).valid[ErrorsList]
    FormValidator.getUser(Map("age" -> "abc", "name" -> " ")) shouldBe
      NonEmptyList
        .of("Field name is empty", "Unable to convert age ('abc') to int: NumberFormatException")
        .invalid[User]
  }

}
