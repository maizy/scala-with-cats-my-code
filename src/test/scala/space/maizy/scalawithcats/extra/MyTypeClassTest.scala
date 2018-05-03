package space.maizy.scalawithcats.extra

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import space.maizy.scalawithcats.BaseSpec

class MyTypeClassTest extends BaseSpec {

  "Implicit from object of the class" should "works" in {
    StaticHelper.getLabel(112) shouldBe "int"
  }

  "Implicit from object of argument" should "works" in {
    val obj = new MySimpleObject("ABC")

    StaticHelper.getLabel(obj) shouldBe "SimpleObject: ABC"
  }

  "Implicit from object with parrent" should "not compile" in {
    val obj = new MyCustomObject("XYZ")
    println(obj)
    "StaticHelper.getLabel(obj)" shouldNot typeCheck
  }

  "Implicit from object with parrent and helper object" should "works" in {
    val obj = new MyCustomObjectOverwrited("YYY")
    StaticHelper.getLabel(obj) shouldBe s"overwrite: YYY-YYY-over"
  }
}
