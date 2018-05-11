package space.maizy.scalawithcats.ch8_case_test_async

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import space.maizy.scalawithcats.BaseSpec
import cats.Id

trait TestUptimeClient extends UptimeClient[Id] {
  def getUptime(hostname: String): Int
}

class TestUptimeClientWithStub(hosts: Map[String, Int]) extends TestUptimeClient {
  def getUptime(hostname: String): Int = hosts.getOrElse(hostname, 0)
}

class UptimeClientTest extends BaseSpec {
  val hosts = Map("host1" -> 10, "host2" -> 6)

  "UptimeClient with HKT" should "works" in {
    val client = new TestUptimeClientWithStub(hosts)
    client.getUptime("host1") shouldBe 10
    client.getUptime("???") shouldBe 0
  }
}
