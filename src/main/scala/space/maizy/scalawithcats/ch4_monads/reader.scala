package space.maizy.scalawithcats.ch4_monads

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.data.Reader
import cats.syntax.applicative._

case class Db(
    usernames: Map[Int, String],
    passwords: Map[String, String]
)

object DbOperations {

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))


  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    findUsername(userId).flatMap { mayBeUserName =>
      mayBeUserName
        .map { userName =>
          checkPassword(userName, password)
        }
        .getOrElse(false.pure[DbReader])
      }

}
