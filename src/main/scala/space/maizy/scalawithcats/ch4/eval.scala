package space.maizy.scalawithcats.ch4

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.Eval

object NaiveEvaluation {
   def factorial(n: BigInt): BigInt =
      if(n == 1) n else n * factorial(n - 1)

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = as match {
    case head :: tail =>
      fn(head, foldRight(tail, acc)(fn))
    case Nil =>
      acc
  }
}

object WithCatsEval {
  def factorialWithoutDefer(n: BigInt): Eval[BigInt] = {
    if (n == 1) Eval.now(1) else factorialWithoutDefer(n - 1).map(_ * n)
  }

  def factorial(n: BigInt): Eval[BigInt] = {
    if (n == 1) Eval.now(1) else Eval.defer(factorial(n - 1).map(_ * n))
  }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] = {

    def internal(xs: List[A], acc: Eval[B]): Eval[B] = {
        xs match {
        case head :: tail =>
          Eval.defer(internal(tail, acc).flatMap(b => Eval.later(fn(head, b))))
        case Nil =>
          acc
      }
    }

    internal(as, Eval.now(acc))
  }

}
