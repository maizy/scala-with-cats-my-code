package space.maizy.scalawithcats.ch5_monad_transformers

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import space.maizy.scalawithcats.BaseSpec

class AutobotTest extends BaseSpec {

  "AutobotTest" should "getPowerLevel" in {
    Await.result(Autobot.getPowerLevel("Jazz").value, Duration.Inf) shouldBe Right(6)
    Await.result(Autobot.getPowerLevel("C3PO").value, Duration.Inf) shouldBe Left("Autobot C3PO not found")
  }

  it should "canSpecialMove" in {
    Await.result(Autobot.canSpecialMove("Hot Rod", "Jazz").value, Duration.Inf) shouldBe Right(true)
    Await.result(Autobot.canSpecialMove("Bumblebee", "Jazz").value, Duration.Inf) shouldBe Right(false)
    Await.result(Autobot.canSpecialMove("Jazz", "Unknown").value, Duration.Inf) shouldBe
      Left("Autobot Unknown not found")
  }

  it should "tacticalReport" in {
    Autobot.tacticalReport("Jazz", "Bumblebee") shouldBe "Jazz and Bumblebee need a recharge."
    Autobot.tacticalReport("Bumblebee", "Hot Rod") shouldBe "Bumblebee and Hot Rod are ready to roll out!"
    Autobot.tacticalReport("Jazz", "Ironhide") shouldBe "Comms error: Autobot Ironhide not found"
  }
}
