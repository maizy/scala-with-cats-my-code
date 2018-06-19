package space.maizy.scalawithcats.ch10_case_data_validation.v5_with_kleisli

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.data.{ Kleisli, NonEmptyList }
import cats.instances.either._
import cats.syntax.either._
import cats.syntax.apply._
import space.maizy.scalawithcats.ch10_case_data_validation.v5_with_kleisli.Predicates.ErrorType

object Predicates {

  type ErrorType = String
  type ErrorsType = NonEmptyList[ErrorType]

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

object Check {
  type Result[A] = Either[Predicates.ErrorsType, A]
  type Check[A, B] = Kleisli[Result, A, B]

  def check[A, B](func: A => Result[B]): Check[A, B] =
    Kleisli(func)

  def checkPred[A](pred: Predicate[ErrorType, A]): Check[A, A] = {
    import PredicateOps._
    Kleisli[Result, A, A](pred.run.apply)
  }
}

object ValidationLib {
  import Check._
  import Predicates._

  def valid(value: String): Result[String] =
    value.asRight[ErrorsType]

  def invalid(error: ErrorType): Result[String] =
    NonEmptyList.one(error).asLeft[String]

  def invalid(error: ErrorType, errors: ErrorType*): Result[String] =
    NonEmptyList.of(error, errors: _*).asLeft[String]

  val userNameCheck: Check[String, String] =
    checkPred(longerThan(3) and alphanumeric)

  /**
   * An email address must contain an @ sign. Split the string at the @.
   * The string to the left􏰃 must not be empty. The string to the right must
   * be at least three characters long and contain a dot.
   */
  private val splitEmail: Check[String, (String, String)] =
    check { email: String =>
      email.split("@").toList match {
        case List(left, right) => (left, right).asRight[ErrorsType]
        case _ => NonEmptyList.one("Value must contains @ with chars before & after it").asLeft[(String, String)]
      }
    }

  private val leftCheck: Check[String, String] = checkPred(longerThan(0))

  private val rightCheck: Check[String, String] =
    checkPred(longerThan(2) and contains('.'))

  private val checkRightLeftAndJoinEmail: Check[(String, String), String] =
    Kleisli { case (left, right) =>
      (leftCheck(left), rightCheck(right)).mapN((l, r) => s"$l@$r")
    }

  val emailCheck: Check[String, String] =
    splitEmail andThen checkRightLeftAndJoinEmail


}
