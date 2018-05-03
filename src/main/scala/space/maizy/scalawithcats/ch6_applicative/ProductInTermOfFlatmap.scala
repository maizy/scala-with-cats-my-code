package space.maizy.scalawithcats.ch6_applicative

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._


object ProductInTermOfFlatmap {

  def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] = {
    for {
      a <- x
      b <- y
    } yield (a, b)
  }
}
