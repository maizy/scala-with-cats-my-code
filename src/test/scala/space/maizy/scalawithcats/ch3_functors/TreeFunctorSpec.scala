package space.maizy.scalawithcats.ch3_functors
/**
  * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
  * See LICENSE.txt for details.
  */

import cats.Functor
import scala.language.higherKinds
import space.maizy.scalawithcats.BaseSpec

class TreeFunctorSpec extends BaseSpec {

  def inc1[F[_]](start: F[Int])(implicit f: Functor[F]): F[Int] = {
    f.map(start)(_ + 1)
  }

  "functors" should "works with abstract logic" in {
    import cats.Id
    import cats.instances.option._
    import cats.syntax.option._
    inc1(2.some) shouldBe Some(3)
    inc1(none[Int]) shouldBe none[Int]

    inc1[Id](4) shouldBe 5 // => type mismatch; found : Int, required: ?F[Int]
  }

  "TreeFunctor" should "works for leaf" in {
    import TreeFunctor._
    treeFunctor.map(Leaf(1))(_ + 1) shouldBe Leaf(2)

  }

  it should "works for tree" in {
    import TreeFunctor._
    val t: Tree[Int] = Branch(Leaf(1), Branch(Leaf(9), Leaf(0)))

    treeFunctor.map(t)(_ - 1) shouldBe Branch(Leaf(0), Branch(Leaf(8), Leaf(-1)))
  }


  it should "works with boxing into std types" in {
    import cats.instances.list._
    import TreeFunctor._

    val ints = List(1, 2, 3)
    Functor[List].map(ints)(_ * 2) shouldBe Seq(2, 4, 6)

    val trees: List[Tree[Int]] = List(Leaf(1), Branch(Leaf(10), Leaf(11)))
    Functor[List].map(trees)(t => Functor[Tree].map(t)(_ * 2)) shouldBe List(Leaf(2), Branch(Leaf(20), Leaf(22)))
  }

  it should "works with contravariant constructors" in {
    import cats.syntax.functor._
    import TreeFunctor._
     import Tree._
    leaf(9).map(i => Math.pow(i.toDouble, 2.0).toInt) shouldBe leaf(81)


    val tree = branch(leaf(1), branch(leaf(3), leaf(5)))

    val processedTree = tree
      .map(_ + 1)
      .map(_ / 2)

    processedTree shouldBe branch(leaf(1), branch(leaf(2), leaf(3)))
  }

  it should "works with abstract logic" in {
    import TreeFunctor._
    import Tree._

    inc1(leaf(5)) shouldBe leaf(6)
    inc1(branch(leaf(1), leaf(0))) shouldBe branch(leaf(2), leaf(1))
  }
}
