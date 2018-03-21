package space.maizy.scalawithcats.ch4

import space.maizy.scalawithcats.BaseSpec

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

class EvalSpec extends BaseSpec {
  val n = 10000
  val longList: List[Int] = Range(0, n).toList

  "Naive factorial" should "fails with stack overflow" in {
    the[StackOverflowError] thrownBy {
      NaiveEvaluation.factorial(n)
    }
  }

  "Factorial without Eval.defer" should "fails with stack overflow" in {
    the[StackOverflowError] thrownBy {
      WithCatsEval.factorialWithoutDefer(n)
    }
  }

  "Factorial with Eval.defer" should "works" in {
    WithCatsEval.factorial(n).value should be >= BigInt(0)
  }

  val foldF: (Int, String) => String = (a, b) => s"($a - $b)"

  "Naive foldRight" should "fails with stack overflow" in {
    the[StackOverflowError] thrownBy {
      NaiveEvaluation.foldRight(longList, "")(foldF)
    }
  }

  "foldRight with Eval" should "works" in {
    WithCatsEval.foldRight(longList, "")(foldF)
  }
}
