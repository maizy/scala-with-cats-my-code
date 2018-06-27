package space.maizy.scalawithcats.ch11_case_crdt.v3_with_storage_abstraction

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import scala.language.higherKinds
import cats.Monoid
import cats.instances.map._
import cats.instances.list._
import cats.syntax.semigroup._
import cats.syntax.foldable._
import space.maizy.scalawithcats.ch11_case_crdt.BoundedSemiLattice

trait GCounter[F[_, _], K, V] {
  def increment(storage: F[K, V])(machine: K, amount: V)(implicit monoid: Monoid[V]): F[K, V]
  def merge(s1: F[K, V], s2: F[K, V])(implicit bsl: BoundedSemiLattice[V]): F[K, V]
  def total(storage: F[K, V])(implicit monoid: Monoid[V]): V
}

object GCounter {
  def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]): GCounter[F, K, V] = counter
}

object GCounterMapInstances {

  implicit def mapGCounter[A]: GCounter[Map, String, A] = new GCounter[Map, String, A] {

    override def increment(storage: Map[String, A])(machine: String, amount: A)
      (implicit monoid: Monoid[A]): Map[String, A] =
      storage.updated(machine, storage.getOrElse(machine, monoid.empty) |+| amount)

    override def merge(s1: Map[String, A], s2: Map[String, A])
      (implicit bsl: BoundedSemiLattice[A]): Map[String, A] =
      s1 |+| s2

    override def total(storage: Map[String, A])(implicit monoid: Monoid[A]): A =
      storage.values.toList.combineAll
  }

}
