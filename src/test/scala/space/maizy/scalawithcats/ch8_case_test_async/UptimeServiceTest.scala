package space.maizy.scalawithcats.ch8_case_test_async

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import space.maizy.scalawithcats.BaseSpec
import cats.Id

class UptimeServiceTest extends BaseSpec {
  val hosts = Map("host1" -> 10, "host2" -> 6)

  val client = new TestUptimeClientWithStub(hosts)

  "UptimeService" should "compile with Id" in {
    val service = new UptimeService[Id](client)
    service.getTotalUptime(hosts.keys.toList) shouldBe hosts.values.sum
  }
}
