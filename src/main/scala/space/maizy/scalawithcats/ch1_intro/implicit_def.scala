package space.maizy.scalawithcats.ch1_intro

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

object ShapePrinter {
  implicit val shapePrinter: Printer[Shape] = new Printer[Shape] {
    override def printMe(value: Shape): String = s"Shape: volume=${value.volume.formatted("%.1f")}"
  }

  implicit val squarePrinter: Printer[Square] = new Printer[Square] {
    override def printMe(value: Square): String =
      s"Square: side=${value.side}, volume=${value.volume.formatted("%.1f")}"
  }
}

// see space.maizy.scalawithcats.ch1.ImplicitDefTest for examples
