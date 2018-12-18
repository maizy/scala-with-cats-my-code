package space.maizy.scalawithcats.ch4_monads

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.data.Writer
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._

object LoggedFactorial {

  def slowly[A](body: => A): A = try body finally Thread.sleep(100L)

  def printLnFactorial(n: Int): Int = {
    val ans = slowly {
      if (n == 0) 1 else n * printLnFactorial(n - 1)
    }
    println(s"${System.currentTimeMillis()}: fact $n $ans")
    ans
  }

  type Logged[A] = Writer[Vector[String], A]

  def loggedFactorial(n: Int): Logged[Int] = {
    for {
      ans <-
        slowly {
          if (n == 0) 1.pure[Logged] else loggedFactorial(n - 1).map(_ * n)
        }
      _ <- Vector(s"${System.currentTimeMillis()}: fact $n $ans").tell
    } yield ans
  }
}
