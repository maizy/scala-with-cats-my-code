package space.maizy.scalawithcats.ch11_case_crdt.v4_with_keyvalue_abstraction

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import scala.language.higherKinds

trait KeyValueStore[F[_,_]] {
  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

  def get[K, V](f: F[K, V])(k: K): Option[V]

  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V = get(f)(k).getOrElse(default)

  def values[K, V](f: F[K, V]): List[V]
}
object KeyValueStore {
  def apply[F[_, _]](implicit k: KeyValueStore[F]): KeyValueStore[F] = k
}

object KeyValueStoreMapInstances {

  implicit def mapKVStorage: KeyValueStore[Map] = new KeyValueStore[Map] {
    override def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f updated(k, v)

    override def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)

    override def values[K, V](f: Map[K, V]): List[V] = f.values.toList
  }
}
