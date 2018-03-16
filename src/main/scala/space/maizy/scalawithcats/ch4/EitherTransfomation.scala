package space.maizy.scalawithcats.ch4

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import scala.collection.immutable.{ Seq => ISeq }

import cats.syntax.either._

object EitherTransfomation {

  def checkNegative(i: Int): Either[String, Int] =
    i.asRight[String].ensure("must be positive or zero")(_ >= 0)

  def sqareNonNegativeInts(ints: ISeq[Int]): ISeq[Either[String, Int]] = {
    ints.map { item =>
      checkNegative(item)
        .map(i => i * i)
        .leftMap(s"Unable to use $item: " + _)
    }
  }

  def divideIntsAndMulBy2(a: Int, b: Int): Either[String, Double] = {
    for {
      eitherA <- a.asRight[String]
      eitherB <- b.asRight[String]
      res <-
        if (b == 0) {
          "Division by zero".asLeft[Double]
        } else {
          (a / b).toDouble.asRight[String]
        }
    } yield res * 2
  }
}
