package space.maizy.scalawithcats.extra

import cats.data.NonEmptyList

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

object NonEmptyListMapAndReduce {

  def mapAndReduceLeft[A, B](value: List[A], mapF: A => B, reduceF: (B, B) => B): B = {
    if (value.isEmpty) {
      throw new NoSuchElementException("Collection is empty")
    }
    val head = mapF(value.head)
    val tail = value.tail.map(mapF)
    tail.foldLeft(head)(reduceF)
  }

  def mapAndReduceLeft2[A, B](value: List[A], mapF: A => B, reduceF: (B, B) => B): B = {
    value.map(mapF).reduceLeft(reduceF)
  }

  def mapAndReduceLeftNEL[A, B](value: NonEmptyList[A], mapF: A => B, reduceF: (B, B) => B): B = {
    value.map(mapF).reduceLeft(reduceF)
  }

}
