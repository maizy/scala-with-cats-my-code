package space.maizy.scalawithcats.ch3

/**
  * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
  * See LICENSE.txt for details.
  */

import cats.Functor

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object TreeFunctor {
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](node: Tree[A])(f: A => B): Tree[B] = node match {
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      case Leaf(v) => Leaf(f(v))
    }
  }
}

