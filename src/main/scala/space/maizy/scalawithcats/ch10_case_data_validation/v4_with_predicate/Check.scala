package space.maizy.scalawithcats.ch10_case_data_validation.v4_with_predicate

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.Semigroup
import cats.data.{ NonEmptyList, ValidatedNel }
import space.maizy.scalawithcats.ch10_case_data_validation.v4_with_predicate.PredicateOps._

// should be sealed, but used in tests for one hack
sealed trait Check[E, A, B] {

  def apply(in: A)(implicit sg: Semigroup[NonEmptyList[E]]): ValidatedNel[E, B]
  def map[C](func: B => C): Check[E, A, C] = Check.Map[E, A, B, C](this, func)
  def flatMap[C](func: B => Check[E, A, C]): Check[E, A, C] = Check.Flatmap[E, A, B, C](this, func)
  def andThen[C](that: Check[E, B, C]): Check[E, A, C] = Check.AndThen[E, A, B, C](this, that)
}

object Check {
  def apply[E, A](predicate: Predicate[E, A]): Check[E, A, A] = Check.PurePredicate(predicate)
  def apply[E, A, B](func: A => ValidatedNel[E, B]): Check[E, A, B] = Check.PureFunc(func)

  final case class Map[E, A, B, C](check: Check[E, A, B], func: B => C) extends Check[E, A, C] {
    override def apply(a: A)(implicit sg: Semigroup[NonEmptyList[E]]): ValidatedNel[E, C] =
      check(a).map(func)
  }

  final case class PurePredicate[E, A](predicate: Predicate[E, A]) extends Check[E, A, A] {
    override def apply(a: A)(implicit sg: Semigroup[NonEmptyList[E]]): ValidatedNel[E, A] =
      predicate.run(a)
  }

  final case class PureFunc[E, A, B](func: A => ValidatedNel[E, B]) extends Check[E, A, B] {
    override def apply(a: A)(implicit sg: Semigroup[NonEmptyList[E]]): ValidatedNel[E, B] =
      func(a)
  }

  final case class Flatmap[E, A, B, C](check: Check[E, A, B], func: B => Check[E, A, C]) extends Check[E, A, C] {
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

  final case class AndThen[E, A, B, C](self: Check[E, A, B], that: Check[E, B, C]) extends Check[E, A, C] {
    override def apply(in: A)(implicit sg: Semigroup[NonEmptyList[E]]): ValidatedNel[E, C] = {
      self(in).withEither(eitherAB => eitherAB.flatMap((b: B) => that(b).toEither))
    }
  }
}

