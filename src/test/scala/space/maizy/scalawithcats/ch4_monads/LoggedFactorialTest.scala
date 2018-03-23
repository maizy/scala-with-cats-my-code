package space.maizy.scalawithcats.ch4_monads

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, Future }
import space.maizy.scalawithcats.BaseSpec
import scala.collection.immutable
import scala.util.{ Failure, Success }

class LoggedFactorialTest extends BaseSpec {
  import scala.concurrent.ExecutionContext.Implicits.global

  "LoggedFactorialTest.printLnFactorial" should "works, but produces messy output" in {
    val futures = Future.sequence(immutable.Seq(
      Future(LoggedFactorial.printLnFactorial(5)),
      Future(LoggedFactorial.printLnFactorial(6))
    ))

    futures.onComplete(println)
    Await.ready(futures, Duration.Inf)
  }

  "LoggedFactorialTest.loggedFactorial" should "works and produce Writable" in {
    val futures = Future.sequence(immutable.Seq(
      Future(LoggedFactorial.loggedFactorial(5)),
      Future(LoggedFactorial.loggedFactorial(6))
    ))

    futures.onComplete {
      case Failure(e) => println(e)
      case Success(r) => r.foreach { resWriter =>
        val (logs, res) = resWriter.run
        println(s"Res: $res\nLogs:\n${logs.mkString("\n")}\n")
      }
    }
    Await.ready(futures, Duration.Inf)
  }

}
