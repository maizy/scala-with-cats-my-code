package space.maizy.scalawithcats.ch10_case_data_validation.v1_either

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.Semigroup

trait Check[E, A] {
  def apply(value: A): Either[E, A]

  def and(that: Check[E, A])(implicit comb: Semigroup[E]): Check[E, A]
}

case class CheckF[E, A](f: A => Either[E, A]) extends Check[E, A] {
  self =>

  override def apply(value: A): Either[E, A] = f(value)

  override def and(that: Check[E, A])(implicit comb: Semigroup[E]): Check[E, A] = {
    val f: A => Either[E, A] = (v: A) => {
      val r1 = self.apply(v)
      val r2 = that.apply(v)
      (r1, r2) match {
        case (Right(_), Right(_)) => Right(v)
        case (Right(_), Left(e2)) => Left(e2)
        case (Left(e1), Right(_)) => Left(e1)
        case (Left(e1), Left(e2)) => Left(comb.combine(e1, e2))
      }
    }
    CheckF(f)
  }
}

