package space.maizy.scalawithcats.ch6_applicative

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import space.maizy.scalawithcats.BaseSpec

class TrySemigroupal extends BaseSpec {

  "Semigroupal" should "works" in {
    import cats.instances.option._
    import cats.Semigroupal

    Semigroupal[Option].product(Some("3"), Some(1)) shouldBe Some(("3", 1))
    Semigroupal[Option].product(None, Some(1)) shouldBe None
  }

  it should "works with nice syntax" in {
    import cats.instances.option._
    import cats.syntax.apply._

    // without explicit type it infers as (Some[String], Some[Int])
    ((Some("3"), Some(1)): (Option[String], Option[Int])).tupled shouldBe Some(("3", 1))

    (None, Some(1)).tupled shouldBe None
  }

  "apply syntax" should "allows to combine Monoids" in {

    import cats.Monoid
    // TODO: less broad imports
    import cats.implicits._

    case class Stat(label: String, count: Int)

    def tupleToStat: (String, Int) => Stat = Stat.apply _
    def statToTuple: Stat => (String, Int) = s => (s.label, s.count)

    implicit val statMonoid: Monoid[Stat] = (
      Monoid[String],
      Monoid[Int]
    ).imapN(tupleToStat)(statToTuple)

    val a = Stat("a", 11)
    val b = Stat("b", 20)

    statMonoid.combine(a, b) shouldBe Stat("ab", 31)
    (a |+| b) shouldBe Stat("ab", 31)
  }

}
