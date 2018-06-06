package space.maizy.scalawithcats.ch10_case_data_validation.v2_adt

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.kernel.Semigroup
import cats.syntax.semigroup._
import cats.syntax.either._

sealed trait Check[E, A] {
  def and(that: Check[E, A]): Check[E, A] = And(this, that)
}

case class And[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]
case class Pure[E, A](f: A => Either[E, A]) extends Check[E, A]

object CheckOps {
  implicit class CheckOps[E, A](check: Check[E, A]) {
    def run(v: A)(implicit s: Semigroup[E]): Either[E, A] = check match {
      case Pure(f) => f(v)
      case And(left, right) =>
        (left.run(v), right.run(v)) match {
          case (Right(_), Right(_)) => v.asRight[E]
          case (Right(_), Left(e2)) => e2.asLeft[A]
          case (Left(e1), Right(_)) => e1.asLeft[A]
          case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft[A]
        }
    }
  }
}
