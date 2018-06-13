package space.maizy.scalawithcats.ch10_case_data_validation.v4_with_predicate

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.Semigroup
import cats.data.{ NonEmptyList, ValidatedNel }
import space.maizy.scalawithcats.ch10_case_data_validation.v4_with_predicate.PredicateOps._

// should be sealed, but used in tests for one hack
trait Check[E, A, B] {
  def apply(in: A)(implicit sg: Semigroup[NonEmptyList[E]]): ValidatedNel[E, B]
  def map[C](func: B => C): Check[E, A, C] = CheckMap[E, A, B, C](this, func)
  def flatMap[C](func: B => Check[E, A, C]): Check[E, A, C] = CheckFlatmap[E, A, B, C](this, func)
}

object Check {
  def apply[E, A](predicate: Predicate[E, A]): Check[E, A, A] = CheckPure(predicate)
}

final case class CheckMap[E, A, B, C](check: Check[E, A, B], func: B => C) extends Check[E, A, C] {
  override def apply(a: A)(implicit sg: Semigroup[NonEmptyList[E]]): ValidatedNel[E, C] =
    check(a).map(func)
}

final case class CheckPure[E, A](predicate: Predicate[E, A]) extends Check[E, A, A] {
  override def apply(a: A)(implicit sg: Semigroup[NonEmptyList[E]]): ValidatedNel[E, A] =
    predicate.run(a)
}

final case class CheckFlatmap[E, A, B, C](check: Check[E, A, B], func: B => Check[E, A, C]) extends Check[E, A, C] {
  override def apply(a: A)(implicit sg: Semigroup[NonEmptyList[E]]): ValidatedNel[E, C] = {
    val resB: ValidatedNel[E, B] = check(a)
    resB.withEither { eitherB: Either[NonEmptyList[E], B] =>
      eitherB.flatMap { bValue =>
        val checkC: Check[E, A, C] = func(bValue)
        checkC(a).toEither
      }
    }
  }
}
