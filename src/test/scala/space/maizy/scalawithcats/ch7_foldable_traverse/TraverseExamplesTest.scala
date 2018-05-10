package space.maizy.scalawithcats.ch7_foldable_traverse

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import space.maizy.scalawithcats.BaseSpec

class TraverseExamplesTest extends BaseSpec {
  "listSequence" should "works for vectors" in {
    import cats.instances.vector._
    (TraverseExamples.listSequence(List(Vector(1, 2), Vector(3, 4))): Vector[List[Int]]) shouldBe
      Vector(
        List(1, 3),
        List(1, 4),
        List(2, 3),
        List(2, 4)
      )

    val lists = TraverseExamples.listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))

    lists.foreach { l =>
      l should have size 3
    }
    lists should have size Math.pow(2.0, 3.0).toLong
  }

  "processListOfIntsWithOption" should "works" in {
    TraverseExamples.processListOfIntsWithOption(List(2, 4, 6)) shouldBe Some(List(2, 4, 6))

    // because flatMap used inside semigroupal combine are fail fast
    TraverseExamples.processListOfIntsWithOption(List(1, 2, 3)) shouldBe Option.empty[List[Int]]
    TraverseExamples.processListOfIntsWithOption(List(2, 1, 3)) shouldBe Option.empty[List[Int]]
    TraverseExamples.processListOfIntsWithOption(List(1, 3, 5)) shouldBe Option.empty[List[Int]]
  }

  "processListOfIntWithValidated" should "works" in {
    import cats.syntax.validated._
    TraverseExamples.processListOfIntWithValidated(List(2, 4, 6)) shouldBe List(2, 4, 6).valid[List[String]]

    // because flatMap used inside semigroupal combine are fail fast
    TraverseExamples.processListOfIntWithValidated(List(1, 2, 3)) shouldBe
        List("1 is not even", "3 is not even").invalid[List[Int]]

    TraverseExamples.processListOfIntWithValidated(List(2, 1, 3)) shouldBe
      List("1 is not even", "3 is not even").invalid[List[Int]]

    TraverseExamples.processListOfIntWithValidated(List(1, 3, 5)) shouldBe
      List("1 is not even", "3 is not even", "5 is not even").invalid[List[Int]]
  }
}
