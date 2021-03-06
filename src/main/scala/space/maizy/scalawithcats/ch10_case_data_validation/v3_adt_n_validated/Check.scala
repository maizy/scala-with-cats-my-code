package space.maizy.scalawithcats.ch10_case_data_validation.v3_adt_n_validated

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.data.{ NonEmptyList, ValidatedNel }
import cats.kernel.Semigroup
import cats.syntax.apply._

sealed trait Check[E, A] {
  def and(that: Check[E, A]): Check[E, A] = And(this, that)
  def or(that: Check[E, A]): Check[E, A] = Or(this, that)
}

case class And[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]
case class Or[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]
case class Pure[E, A](f: A => ValidatedNel[E, A]) extends Check[E, A]

object CheckOps {
  implicit class CheckOps[E, A](check: Check[E, A]) {
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
