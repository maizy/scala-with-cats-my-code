package space.maizy.scalawithcats.ch7_foldable_traverse

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

object ListMethodsFromFoldRight {

  implicit class CommonMethodImpl[T](list: List[T]) {
    def frMap[U](f: T => U): List[U] =
      list.foldRight(List.empty[U]) { (i, acc) =>
        f(i) :: acc
      }

    def frFlatmap[U](f: T => List[U]): List[U] =
      list.foldRight(List.empty[U]) { (i, acc) =>
        f(i) ::: acc
      }

    def frFilter(f: T => Boolean): List[T] =
      list.foldRight(List.empty[T]) { (i, acc) =>
        if (f(i)) {
          i :: acc
        } else {
          acc
        }
      }
  }

  implicit class NumericMethods[T](list: List[T])(implicit ops: Numeric[T]) {
    def frSum: T =
      list.foldRight(ops.zero) { (i, acc) =>
        ops.plus(acc, i)
      }
  }
}
