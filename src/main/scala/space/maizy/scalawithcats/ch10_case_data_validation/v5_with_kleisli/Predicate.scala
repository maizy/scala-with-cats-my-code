package space.maizy.scalawithcats.ch10_case_data_validation.v5_with_kleisli

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.data.{ NonEmptyList, ValidatedNel }
import cats.kernel.Semigroup
import cats.syntax.apply._
import cats.syntax.validated._

sealed trait Predicate[E, A] {
  def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)
  def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)
}

final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
final case class Pure[E, A](f: A => ValidatedNel[E, A]) extends Predicate[E, A]

object PredicateOps {
  implicit class CheckOps[E, A](predicate: Predicate[E, A]) {
    def toValidated(v: A)(implicit sg: Semigroup[NonEmptyList[E]]): ValidatedNel[E, A] =
      predicate match {
        case Pure(f) => f(v)

        case And(left, right) =>
          (left.toValidated(v), right.toValidated(v)).mapN((_, _) => v)

        case Or(left, right) =>
          left.toValidated(v) findValid right.toValidated(v)
      }

    def run(implicit sg: Semigroup[NonEmptyList[E]]): A => Either[NonEmptyList[E], A] =
      toValidated _ andThen (_.toEither)
  }
}

object Predicate {
  def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] =
    Pure(a => if (fn(a)) a.validNel[E] else err.invalidNel[A])
}
