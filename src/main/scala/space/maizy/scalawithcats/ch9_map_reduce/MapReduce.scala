package space.maizy.scalawithcats.ch9_map_reduce

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import scala.concurrent.Future
import cats.Monoid
import cats.instances.future._
import cats.instances.vector._
import cats.syntax.traverse._
import cats.syntax.monoid._
import cats.syntax.foldable._

object MapReduce {

  final val CPUs = Runtime.getRuntime.availableProcessors
  import scala.concurrent.ExecutionContext.Implicits.global

  def foldMap[A, B: Monoid](seq: Vector[A])(f: A => B): B =
    seq.foldLeft(Monoid[B].empty) { (acc, i) =>
      acc |+| f(i)
    }

  def parallelFoldMap[A, B : Monoid](seq: Vector[A])(f: A => B): Future[B] = {
    val batchSize = (seq.length.toDouble / CPUs).ceil.toInt
    val batches = seq.grouped(batchSize).toVector
    val computationParts: Future[Vector[B]] = batches
      .map(batch => Future(foldMap(batch)(f)))
      .sequence

    computationParts.map(batches => foldMap(batches)(identity))
  }

  def parallelFoldMapWithCats[A, B : Monoid](seq: Vector[A])(f: A => B): Future[B] = {
    val batchSize = (seq.length.toDouble / CPUs).ceil.toInt
    val batches = seq.grouped(batchSize).toVector

    batches
      .traverse(batch => Future(batch.foldMap(f)))
      .map(_.combineAll)
  }
}
