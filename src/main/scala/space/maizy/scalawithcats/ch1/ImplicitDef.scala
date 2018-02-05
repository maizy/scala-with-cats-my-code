package space.maizy.scalawithcats.ch1

trait Printer[T] {
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

object ImplicitDef {

  def main(args: Array[String]): Unit = {
    import StdTypePrinters._

    println(Printer.printMe(5))
    println(Printer.printMe("test"))

  }
}
