package space.maizy.scalawithcats.ch4_monads

/**
  * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
  * See LICENSE.txt for details.
  */

import scala.language.higherKinds

trait MyMonad[F[_]] {
  self =>
  def pure[A](a: A): F[A]
  def flatMap[A, B](value: F[A])(f: A => F[B]): F[B]

  def map[A, B](value: F[A])(f: A => B): F[B] =
    self.flatMap(value)(v => self.pure[B](f(v)))
}

final case class Box[T](v: T)

object MyMonadInstances {
  val boxMonad: MyMonad[Box] = new MyMonad[Box] {
    override def pure[A](a: A): Box[A] = Box(a)
    override def flatMap[A, B](value: Box[A])(f: A => Box[B]): Box[B] = f(value.v)
  }

  type MyId[A] = A

  val idMonad: MyMonad[MyId] = new MyMonad[MyId] {
    override def pure[A](a: A): MyId[A] = a
    override def flatMap[A, B](value: MyId[A])(f: A => MyId[B]): MyId[B] = f(value)
    override def map[A, B](value: MyId[A])(f: A => B): MyId[B] = flatMap(value)(f)
  }
}
