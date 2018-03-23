package space.maizy.scalawithcats.ch1_intro

/**
  * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
  * See LICENSE.txt for details.
  */

import space.maizy.scalawithcats.BaseSpec


class ImplicitDefSpec extends BaseSpec {

  "Printer" should "works" in {
    import StdTypePrinters._
    import OptPrinter._

    println(Printer.printMe(5))
    println(Printer.printMe("test"))
    println(Printer.printMe(Some("test")))
    println(Printer.printMe(Option.empty[Int]))
    println(Printer.printMe(None))
    println(Printer.printMe(Some(7)))

    Printer.printMe(5) shouldBe "Int: 5"
    Printer.printMe("test") shouldBe "Str: test"
  }

  it should "work as specialization for dep types" in {
    import ShapePrinter._
    Printer.printMe(new Shape(5.0)) shouldBe "Shape: volume=5.0"
    Printer.printMe(new Square(2.0))(squarePrinter) shouldBe "Square: side=2.0, volume=4.0"

    // TODO not working, shape printer choosed
    //Printer.printMe(new Square(2.0)) shouldBe "Square: side=2.0, volume=4.0"

    Printer.printMe(new Circle(8.0)) shouldBe "Shape: volume=201.1"
  }
}
