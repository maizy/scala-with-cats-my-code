package space.maizy.scalawithcats.ch10_case_data_validation.v4_with_predicate

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.data.{ NonEmptyList, ValidatedNel }
import cats.Semigroup
import PredicateOps._

sealed trait Check[E, A, B] {
  def apply(a: A)(implicit sg: Semigroup[NonEmptyList[E]]): ValidatedNel[E, B]
  def map[C](func: B => C): Check[E, A, C] = CheckMap[E, A, B, C](this, func)
}

object Check {
  def apply[E, A, B](predicate: Predicate[E, A]) = CheckPure(predicate)
}

final case class CheckMap[E, A, B, C](check: Check[E, A, B], func: B => C) extends Check[E, A, C] {
  override def apply(a: A)(implicit sg: Semigroup[NonEmptyList[E]]): ValidatedNel[E, C] =
    check(a).map(func)
}

final case class CheckPure[E, A](predicate: Predicate[E, A]) extends Check[E, A, A] {
  override def apply(a: A)(implicit sg: Semigroup[NonEmptyList[E]]): ValidatedNel[E, A] =
    predicate.run(a)
}
