package space.maizy.scalawithcats.сh6_applicative

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.data.{ NonEmptyList, Validated }

case class User(name: String, age: Int)

/**
 *  - the name and age must be specified;
 *  - the name must not be blank;
 *  - the age must be a valid non negative integer.
 */
object FormValidator {
  type FirstErrorOr[A] = Either[NonEmptyList[String], A]
  type AllErrosOr[A] = Validated[NonEmptyList[String], A]
  type FormData = Map[String, String]

  def readName(data: FormData): FirstErrorOr[String] = ???

  def readAge(data: FormData): FirstErrorOr[Int] = ???

  private[сh6_applicative] def getValue(field: String, data: FormData): FirstErrorOr[String] =
    data.get(field).toRight(NonEmptyList.one(s"$field is required"))
}
