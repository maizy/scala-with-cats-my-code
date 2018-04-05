package space.maizy.scalawithcats.ch5_monad_transformers

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, Future }
import cats.data.EitherT
import cats.instances.future._

object Autobot {
  import scala.concurrent.ExecutionContext.Implicits.global
  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz"      -> 6,
    "Bumblebee" -> 8,
    "Hot Rod"   -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] = {
    powerLevels.get(autobot) match {
      case Some(level) => EitherT.right(Future(level))
      case None => EitherT.left(Future(s"Autobot $autobot not found"))
    }
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      l1 <- getPowerLevel(ally1)
      l2 <- getPowerLevel(ally2)
    } yield l1 + l2 > 15

  def tacticalReport(ally1: String, ally2: String): String = {
    val future = canSpecialMove(ally1, ally2).value
    Await.result(future, Duration.Inf) match {
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) => s"$ally1 and $ally2 need a recharge."
      case Left(error) => s"Comms error: $error"
    }
  }

}
