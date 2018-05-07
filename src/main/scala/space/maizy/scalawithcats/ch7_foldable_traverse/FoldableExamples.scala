package space.maizy.scalawithcats.ch7_foldable_traverse

import cats.{ Eval, Foldable }
import cats.instances.stream._

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

object FoldableExamples {

  def nativeFoldableAvg(nums: Stream[Long]): Double = {
    // here one may use foldLeft, but foldRight used to emulate StackOverflowEx.
    val (sum, n) = nums.foldRight((0L, 0L)) {
      case (i, (acc, cnt)) => (acc + i, cnt + 1L)
    }
    sum.toDouble / n
  }

  def foldableAvg(nums: Stream[Long]): Eval[Double] = {
    Foldable[Stream].foldRight(nums, Eval.now((0L, 0L))) {
      (i, eval) => eval.map{ case (acc, cnt) => (acc + i, cnt + 1L) }
    }.map { case (sum, n) =>
      sum.toDouble / n
    }
  }
}
