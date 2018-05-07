package space.maizy.scalawithcats.ch7_foldable_traverse

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import space.maizy.scalawithcats.BaseSpec

class FoldableExamplesTest extends BaseSpec {
  "nativeFoldableAvg" should "works" in {
    val stream = List.fill(100)(10L).toStream
    FoldableExamples.nativeFoldableAvg(stream) shouldBe 10.0
  }

  it should "fails with StackOverflow" in {
    val stream = List.fill(100000)(10L).toStream
    the[StackOverflowError] thrownBy {
      FoldableExamples.nativeFoldableAvg(stream)
    }
  }

  "foldableAvg" should "works" in {
    val stream = List.fill(100)(10L).toStream
    FoldableExamples.foldableAvg(stream).value shouldBe 10.0
  }

  it should "not fail with StackOverflow" in {
    val stream = List.fill(100000)(10L).toStream
    FoldableExamples.foldableAvg(stream).value shouldBe 10.0
  }
}
