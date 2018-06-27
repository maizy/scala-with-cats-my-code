package space.maizy.scalawithcats.ch11_case_crdt.v4_with_keyvalue_abstraction

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import scala.language.higherKinds
import cats.Monoid
import cats.syntax.semigroup._
import cats.syntax.foldable._
import cats.instances.list._
import space.maizy.scalawithcats.ch11_case_crdt.BoundedSemiLattice
import space.maizy.scalawithcats.ch11_case_crdt.v3_with_storage_abstraction.GCounter

object GcounterGenerators {
  import KeyValueStore.KvsOps

  implicit def gcounterInstance[F[_, _], K, V]
    (implicit s: KeyValueStore[F], km: Monoid[F[K, V]]): GCounter[F, K, V] = new GCounter[F, K, V] {

    override def increment(storage: F[K, V])(machine: K, amount: V)(implicit monoid: Monoid[V]): F[K, V] = {
      val newAmount = storage.getOrElse(machine, monoid.empty) |+| amount
      storage.put(machine, newAmount)
    }

    override def merge(s1: F[K, V], s2: F[K, V])(implicit bsl: BoundedSemiLattice[V]): F[K, V] =
      s1 |+| s2

    override def total(storage: F[K, V])(implicit monoid: Monoid[V]): V =
      storage.values.combineAll
  }
}
