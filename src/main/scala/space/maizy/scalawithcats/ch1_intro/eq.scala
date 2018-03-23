package space.maizy.scalawithcats.ch1_intro

/**
  * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
  * See LICENSE.txt for details.
  */

import cats.Eq

final case class Cat(name: String, age: Int, color: String)

object CatEq {
  implicit val catEq: Eq[Cat] = Eq.fromUniversalEquals
}

object EqTest {
  def main(args: Array[String]): Unit = {
    import cats.syntax.eq._
    import CatEq._
    val c1 = Cat("a", 5, "white")
    val c2 = Cat("a", 5, "white")
    val c3 = Cat("b", 5, "white")

    assert(c1 === c2, "c1 === c2")
    assert(c1 =!= c3, "c1 =!= c3")
    assert(catEq.eqv(c1, c2), ".eqv")
  }
}
