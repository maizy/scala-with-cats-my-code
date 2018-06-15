package space.maizy.scalawithcats.ch10_case_data_validation.v4_with_predicate

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.data.{ NonEmptyList, Validated, ValidatedNel }
import cats.syntax.validated._
import cats.syntax.apply._
import space.maizy.scalawithcats.ch10_case_data_validation.v4_with_predicate.Predicates.ErrorType

object Predicates {

  type ErrorType = String

  def longerThan(n: Int): Predicate[ErrorType, String] =
    Predicate.lift(
      s"Must be longer than $n characters",
      str => str.length > n
    )

  val alphanumeric: Predicate[ErrorType, String] =
    Predicate.lift(
      "Must be all alphanumeric characters",
      str => str.forall(_.isLetterOrDigit)
    )

  def contains(char: Char): Predicate[ErrorType, String] =
    Predicate.lift(
      s"Must contain the character $char",
      str => str.contains(char)
    )

  def containsOnce(char: Char): Predicate[ErrorType, String] =
    Predicate.lift(
      s"Must contain the character $char only once",
      str => str.count(c => c == char) == 1
    )
}

object ValidationLib {
  import Predicates._

  def valid(value: String): ValidatedNel[ErrorType, String] =
    value.validNel[ErrorType]

  def invalid(error: ErrorType): ValidatedNel[ErrorType, String] =
    error.invalidNel[String]

  def invalid(error: ErrorType, errors: ErrorType*): Validated[NonEmptyList[ErrorType], String] =
    NonEmptyList.of(error, errors: _*).invalid[ErrorType]

  val userNameCheck: Check[ErrorType, String, String] =
    Check(longerThan(3) and alphanumeric)

  /**
   * An email address must contain an @ sign. Split the string at the @.
   * The string to the left􏰃 must not be empty. The string to the right must
   * be at least three characters long and contain a dot.
   */

  private val splitEmail: Check[ErrorType, String, (String, String)] =
    Check { email: String =>
      email.split("@").toList match {
        case List(left, right) => (left, right).validNel
        case _ => "Value must contains @ with chars before & after it".invalidNel[(String, String)]
      }
    }

  private val leftCheck: Check[ErrorType, String, String] =
    Check(longerThan(0))

  private val rightCheck: Check[ErrorType, String, String] =
    Check(longerThan(2) and contains('.'))

  private val checkRightLeftAndJoinEmail: Check[ErrorType, (String, String), String] =
    Check { case (left, right) =>
      (leftCheck(left), rightCheck(right)).mapN((l, r) => s"$l@$r")
    }

  val emailCheck: Check[ErrorType, String, String] =
    splitEmail andThen checkRightLeftAndJoinEmail


}
