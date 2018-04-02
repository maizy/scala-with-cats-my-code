package space.maizy.scalawithcats.ch4_monads

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import space.maizy.scalawithcats.BaseSpec

class PostOrderCalculatorTest extends BaseSpec {

  import PostOrderCalculator._

  "PostOrderCalculator.evalOne" should "works" in {
    evalOne("+").runA(List(1, 2)).value shouldBe 3
    evalOne("-").runA(List(3, 5)).value shouldBe 2
    evalOne("/").runA(List(2, 8)).value shouldBe 4
    evalOne("*").runA(List(2, 4)).value shouldBe 8

    PostOrderCalculator.evalOne("5").run(List(2)).value shouldBe((List(5, 2), 5))
  }

  it should "supports simple program" in {
    val program = for {
      _ <- evalOne("1")
      _ <- evalOne("2")
      ans <- evalOne("+")
    } yield ans

    program.runA(Nil).value shouldBe 3
  }

  "PostOrderCalculator.evalAll" should "works" in {
    val program = evalAll(List("1", "2", "+", "3", "*"))
    program.runA(Nil).value shouldBe 9
  }

  it should "works in complex program" in {
    val program = for {
      _   <- evalAll(List("1", "2", "+"))
      _   <- evalAll(List("3", "4", "+"))
      ans <- evalOne("*")
    } yield ans

    program.runA(Nil).value shouldBe 21
  }

}
