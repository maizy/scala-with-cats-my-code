package space.maizy.scalawithcats.ch4_monads

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.MonadError
import cats.instances.either._
import cats.syntax.applicative._

object CheckFormErrors {

  type ErrorOr[A] = Either[String, A]

  case class FormData(
    email: String,
  )

  case class ParsedFormData(
    login: String,
  )

  private val monadError = MonadError[ErrorOr, String]

  def check(formData: FormData): ErrorOr[ParsedFormData] =
    monadError.ensure(ParsedFormData(formData.email).pure)("Wrong email")(_.login.contains("@"))

  def checkAndCheckLength(formData: FormData): ErrorOr[ParsedFormData] =
    monadError.ensure(check(formData))("Too short login")(_.login.length > 5)

  def checkAndRecover(formData: FormData): ErrorOr[ParsedFormData] =
    monadError.handleError(check(formData)) {
      case "Wrong email" => ParsedFormData("boss@example.com")
    }

  def alwaysFailedCheck(): ErrorOr[ParsedFormData] = monadError.raiseError("Always Failed")

  def alwaysSuccessfullCheck(): ErrorOr[ParsedFormData] =
    monadError.pure(ParsedFormData("test@example.com"))
}
