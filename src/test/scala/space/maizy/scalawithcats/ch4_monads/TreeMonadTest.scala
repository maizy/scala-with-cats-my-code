package space.maizy.scalawithcats.ch4_monads

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import space.maizy.scalawithcats.BaseSpec

class TreeMonadTest extends BaseSpec {
  "TreeMonad" should "works" in {
    import cats.syntax.flatMap._
    import cats.syntax.functor._

    import space.maizy.scalawithcats.ch3_functors.Tree._
    import TreeMonad._

    val res = for {
      a <- branch(leaf("A"), leaf("B"))
      b <- branch(leaf(a + "!"), leaf(a + "?"))
    } yield b

    res shouldBe branch(
      branch(leaf("A!"), leaf("A?")),
      branch(leaf("B!"), leaf("B?"))
    )
  }
}
