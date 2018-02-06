package space.maizy.scalawithcats.ch1

trait Printer[-T] {
  def printMe(value: T): String
}

object Printer {
  def printMe[T](value: T)(implicit printer: Printer[T]): String =
    printer.printMe(value)
}

object StdTypePrinters {
  implicit val intPrinter: Printer[Int] = new Printer[Int] {
    override def printMe(value: Int): String = s"Int: $value"
  }

  implicit val strPrinter: Printer[String] = new Printer[String] {
    override def printMe(value: String): String = s"Str: $value"
  }
}

object OptPrinter {

  implicit def optPrinter[T](implicit valuePrinter: Printer[T]): Printer[Option[T]] =
    new Printer[Option[T]] {
      override def printMe(value: Option[T]): String = {
        value match {
          case Some(v) => "Some[" + valuePrinter.printMe(v) + "]"
          case None => s"None"
        }
      }
    }

  implicit def nonePrinter: Printer[None.type] =
    new Printer[None.type] {
      override def printMe(value: None.type): String = "None!"
    }
}

object ImplicitDef {

  def main(args: Array[String]): Unit = {
    import StdTypePrinters._
    import OptPrinter._

    println(Printer.printMe(5))
    println(Printer.printMe("test"))
    println(Printer.printMe(Some("test")))
    println(Printer.printMe(Option.empty[Int]))
    println(Printer.printMe(None))
    println(Printer.printMe(Some(7)))

  }
}
