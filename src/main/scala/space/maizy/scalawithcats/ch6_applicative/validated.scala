package space.maizy.scalawithcats.ch6_applicative

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.data.{ NonEmptyList, Validated }
import cats.syntax.either._
import cats.syntax.apply._

case class User(name: String, age: Int)

/**
 *  - the name and age must be specified;
 *  - the name must not be blank;
 *  - the age must be a valid non negative integer.
 */
object FormValidator {
  type ErrorsList = NonEmptyList[String]
  type FirstErrorOr[A] = Either[ErrorsList, A]
  type AllErrorsOr[A] = Validated[ErrorsList, A]
  type FormData = Map[String, String]

  /**
   * N.B. Intellij IDEA 2017.3.4 couldn't resolve mapN method
   *      see https://youtrack.jetbrains.com/issue/SCL-12892
   */
  def getUser(data: FormData): AllErrorsOr[User] =
    (
      readName(data).toValidated,
      readAge(data).toValidated
    ).mapN(User.apply)

  def readName(data: FormData): FirstErrorOr[String] =
    for {
      name <- getValue("name")(data)
      trimmedName <- nonBlank("name")(name)
    } yield trimmedName

  def readAge(data: FormData): FirstErrorOr[Int] =
    for {
      ageString <- getValue("age")(data)
      ageNonEmpty <- nonBlank("age")(ageString)
      ageInt <- parseInt("age")(ageNonEmpty)
      age <- nonNegative("age")(ageInt)
    } yield age

  private[ch6_applicative] def getValue(field: String)(data: FormData): FirstErrorOr[String] =
    data.get(field).toRight(NonEmptyList.one(s"$field is required"))

  private[ch6_applicative] def parseInt(field: String)(value: String): FirstErrorOr[Int] =
    Either
      .catchOnly[NumberFormatException](value.toInt)
      .leftMap(e => NonEmptyList.one(s"Unable to convert $field ('$value') to int: ${e.getClass.getSimpleName}"))

  private[ch6_applicative] def nonBlank(field: String)(value: String): FirstErrorOr[String] =
    value.trim.asRight[ErrorsList]
      .ensure(NonEmptyList.one(s"Field $field is empty"))(_.length > 0)

  private[ch6_applicative] def nonNegative(field: String)(value: Int): FirstErrorOr[Int] =
    value.asRight[ErrorsList]
      .ensure(NonEmptyList.one(s"Field $field is less than 0"))(_ >= 0)

}
