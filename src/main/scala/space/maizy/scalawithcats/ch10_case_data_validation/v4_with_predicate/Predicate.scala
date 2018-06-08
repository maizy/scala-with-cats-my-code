package space.maizy.scalawithcats.ch10_case_data_validation.v4_with_predicate

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.data.{ NonEmptyList, ValidatedNel }
import cats.kernel.Semigroup
import cats.syntax.apply._

sealed trait Predicate[E, A] {
  def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)
  def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)
}

final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
final case class Pure[E, A](f: A => ValidatedNel[E, A]) extends Predicate[E, A]

object PredicateOps {
  implicit class CheckOps[E, A](check: Predicate[E, A]) {
    def run(v: A)(implicit sg: Semigroup[NonEmptyList[E]]): ValidatedNel[E, A] =
      check match {
        case Pure(f) => f(v)

        case And(left, right) =>
          (left.run(v), right.run(v)).mapN((_, _) => v)

        case Or(left, right) =>
          left.run(v) findValid right.run(v)
      }
  }
}
