package space.maizy.scalawithcats.ch8_case_test_async

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */


import scala.language.higherKinds
import cats.Applicative

import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.functor._

class UptimeService[F[_]](client: UptimeClient[F])(implicit ap: Applicative[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}
