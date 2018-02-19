package space.maizy.scalawithcats.ch2

/**
  * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
  * See LICENSE.txt for details.
  */

import org.scalatest.{ FlatSpec, Matchers }
import cats.Monoid

class BooleanMonoidSpec extends FlatSpec with Matchers {

  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean =
      m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)

  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean =
      (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)


  "BooleanMonoid" should "supports associative law" in {
    for (x <- Seq(true, false)) {
      for (y <- Seq(true, false)) {
        for (z <- Seq(true, false)) {
          associativeLaw(x, y, z)(BooleanMonoid) shouldBe true
        }
      }
    }
  }

  it should "supports identity law" in {
    identityLaw(true)(BooleanMonoid) shouldBe true
  }
}
