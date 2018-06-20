package space.maizy.scalawithcats.extra

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import cats.data.NonEmptyList
import space.maizy.scalawithcats.BaseSpec

class NonEmptyListMapAndReduceTest extends BaseSpec {

  // emulation
  case class DataFrame(v: String) {
    def union(other: DataFrame): DataFrame = DataFrame(v + " union " + other.v)
  }

  val sample = List(1, 2, 3)

  val makeDataFrame = (v: Int) => DataFrame(s"df$v")

  val unionDataFrame = (a: DataFrame, b:DataFrame) => a union b

  "mapAndReduceLeftNonEmpty" should "works" in {
    NonEmptyListMapAndReduce.mapAndReduceLeft(
      sample,
      makeDataFrame,
      unionDataFrame
    ) shouldBe DataFrame("df1 union df2 union df3")
  }

  it should "raise exception if list is empty" in {
    the[NoSuchElementException] thrownBy {
      NonEmptyListMapAndReduce.mapAndReduceLeft(
        List.empty[Int],
        makeDataFrame,
        unionDataFrame
      )
    }
  }

  "mapAndReduceLeftNonEmpty2" should "works" in {
    NonEmptyListMapAndReduce.mapAndReduceLeft2(
      sample,
      makeDataFrame,
      unionDataFrame
    ) shouldBe DataFrame("df1 union df2 union df3")
  }

  it should "raise exception if list is empty" in {
    the[UnsupportedOperationException] thrownBy {
      NonEmptyListMapAndReduce.mapAndReduceLeft2(
        List.empty[Int],
        makeDataFrame,
        unionDataFrame
      )
    }
  }

  "mapAndReduceLeftNEL" should "works" in {
    NonEmptyListMapAndReduce.mapAndReduceLeftNEL(
      NonEmptyList.of(sample.head, sample.tail: _*),
      makeDataFrame,
      unionDataFrame
    ) shouldBe DataFrame("df1 union df2 union df3")
  }
}
