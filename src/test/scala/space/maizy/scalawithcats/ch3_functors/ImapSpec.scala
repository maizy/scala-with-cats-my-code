package space.maizy.scalawithcats.ch3_functors

/**
  * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
  * See LICENSE.txt for details.
  */

import space.maizy.scalawithcats.BaseSpec

class ImapSpec extends BaseSpec {
  "Monoid for Symbol builded by imap" should "works" in {
    import cats.Monoid
    import cats.syntax.semigroup._
    import SymbolMonoid._

    Monoid[Symbol].empty should equal (Symbol(""))
    'hello |+| 'world should equal ('helloworld)
  }
}
