package space.maizy.scalawithcats.ch4_monads

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.data.State
import cats.syntax.applicative._

class PostOrderCalculatorError(msg: String) extends RuntimeException(msg)

object PostOrderCalculator {
  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] =
    State[List[Int], Int] { stack =>
      if (sym.matches("[0-9]+")) {
        (sym.toInt :: stack, sym.toInt)
      } else {
        sym match {
          case "+" => operator(stack, _ + _)
          case "-" => operator(stack, _ - _)
          case "/" => operator(stack, _ / _)
          case "*" => operator(stack , _ * _)
          case s: String => throw new PostOrderCalculatorError(s"Unknown token '$s'")
        }
      }
    }

  private def popPair(stack: List[Int]): (Int, Int) = {
    if (stack.size < 2) {
      throw new PostOrderCalculatorError("Stack contains less than two elements")
    }
    (stack.tail.head, stack.head)
  }

  private def operator(stack: List[Int], f: (Int, Int) => Int): (List[Int], Int) = {
    val (a, b) = popPair(stack)
    val newStack = stack.drop(2)
    val r = f(a, b)
    (r :: newStack) -> r
  }

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (acc, i) =>
      acc.flatMap(_ => evalOne(i))
    }

  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value
}
