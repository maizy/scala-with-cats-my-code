package space.maizy.scalawithcats.ch4_monads

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import space.maizy.scalawithcats.BaseSpec

class DbOperationsTest extends BaseSpec {

  "DbOperations" should "checkLogin" in {
    val users = Map(
      1 -> "dade",
      2 -> "kate",
      3 -> "margo"
    )
    val passwords = Map(
      "dade" -> "zerocool",
      "kate" -> "acidburn",
      "margo" -> "secret"
    )

    val db = Db(users, passwords)

    DbOperations.checkLogin(1, "zerocool").run(db) shouldBe true
    DbOperations.checkLogin(1, "123456").run(db) shouldBe false
    DbOperations.checkLogin(4, "davinci").run(db) shouldBe false

  }

}
