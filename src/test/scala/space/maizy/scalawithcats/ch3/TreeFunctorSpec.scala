package space.maizy.scalawithcats.ch3
/**
  * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
  * See LICENSE.txt for details.
  */

import org.scalatest.{ FlatSpec, Matchers }
import cats.Functor
import scala.language.higherKinds

class TreeFunctorSpec extends FlatSpec with Matchers {

  def inc1[F[_]](start: F[Int])(implicit f: Functor[F]): F[Int] = {
    f.map(start)(_ + 1)
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

  it should "works with abstract logic" in {
    import cats.instances.option._
    import cats.syntax.option._
    inc1(2.some) shouldBe Some(3)
    inc1(none[Int]) shouldBe none[Int]
  }

  it should "works with boxing into std types" in {
    import cats.instances.list._
    import cats.Functor
    import TreeFunctor._

    val ints = List(1, 2, 3)
    Functor[List].map(ints)(_ * 2) shouldBe Seq(2, 4, 6)

    val trees: List[Tree[Int]] = List(Leaf(1), Branch(Leaf(10), Leaf(11)))
    Functor[List].map(trees)(t => Functor[Tree].map(t)(_ * 2)) shouldBe List(Leaf(2), Branch(Leaf(20), Leaf(22)))
  }

  it should "works with contravariant constructors" in {
    import cats.syntax.functor._
    import TreeFunctor._
    Tree.leaf(9).map(i => Math.pow(i.toDouble, 2.0).toInt) shouldBe Tree.leaf(81)


    val tree = Tree.branch(Tree.leaf(1), Tree.branch(Tree.leaf(3), Tree.leaf(5)))

    val processedTree = tree
      .map(_ + 1)
      .map(_ / 2)

    processedTree shouldBe Tree.branch(Tree.leaf(1), Tree.branch(Tree.leaf(2), Tree.leaf(3)))
  }
}
