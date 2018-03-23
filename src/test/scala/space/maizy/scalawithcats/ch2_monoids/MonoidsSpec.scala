package space.maizy.scalawithcats.ch2_monoids

/**
  * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
  * See LICENSE.txt for details.
  */

import cats.Monoid
import space.maizy.scalawithcats.BaseSpec

class MonoidsSpec extends BaseSpec {

  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean =
      m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)

  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean =
      (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)

  val booleanMonoids = Seq(
    BooleanMonoids.andMonoid,
    BooleanMonoids.orMonoid,
    BooleanMonoids.xorMonoid,
    BooleanMonoids.norMonoid
  )

  "BooleanMonoid" should "supports associative law" in {

    for (m <- booleanMonoids) {
      for (x <- Seq(true, false)) {
        for (y <- Seq(true, false)) {
          for (z <- Seq(true, false)) {
            associativeLaw(x, y, z)(m) shouldBe true
          }
        }
      }
    }
  }

  it should "supports identity law" in {
    for (m <- booleanMonoids) {
      identityLaw(x = true)(m) shouldBe true
      identityLaw(x = false)(m) shouldBe true
    }
  }

  val setMonoids: Seq[Monoid[Set[Int]]] = Seq(
    SetMonoids.appendMonoid
  )

  "Set monoids" should "supports associative law" in {

    for (m <- setMonoids) {
      for (x <- Seq(Set.empty[Int], Set(1, 2, 3))) {
        for (y <- Seq(Set.empty[Int], Set(3, 5, 7))) {
          for (z <- Seq(Set(9, 10, 99))) {
            associativeLaw(x, y, z)(m) shouldBe true
          }
        }
      }
    }
  }

  it should "supports identity law" in {
    for (m <- setMonoids) {
      identityLaw(x = Set.empty[Int])(m) shouldBe true
      identityLaw(x = Set(1, 2, 3))(m) shouldBe true
    }
  }

  "cats Monoid" should "works" in {
    import cats.instances.string._
    import cats.instances.option._
    import cats.syntax.semigroup._
    import cats.syntax.option._

    ("a" |+| "b") shouldBe "ab"
    ("a".some |+| none[String]) shouldBe Some("a")
  }
}
