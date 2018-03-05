package space.maizy.scalawithcats.ch3

/**
  * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
  * See LICENSE.txt for details.
  */

import org.scalatest.{ FlatSpec, Matchers }

class ImapSpec extends FlatSpec with Matchers {
  "Monoid for Symbol builded by imap" should "works" in {
    import cats.Monoid
    import cats.syntax.semigroup._
    import SymbolMonoid._

    Monoid[Symbol].empty should equal (Symbol(""))
    'hello |+| 'world should equal ('helloworld)
  }
}
