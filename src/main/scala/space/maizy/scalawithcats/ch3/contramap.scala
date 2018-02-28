package space.maizy.scalawithcats.ch3

/**
  * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
  * See LICENSE.txt for details.
  */

trait Printable[A] {
  def format(v: A): String
  def contramap[B](func: B => A)(implicit p: Printable[A]): Printable[B] = new Printable[B] {
    override def format(v: B): String = Printable.format(func(v))
  }
}

object Printable {
  def format[A](v: A)(implicit p: Printable[A]): String = p.format(v)
}

final case class Box[A](value: A)

object PrintableInstances {
  implicit val stringPrintable: Printable[String] = new Printable[String] {
    override def format(v: String): String = "\"" + v + "\""
  }

  implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
    override def format(v: Boolean): String = if (v) "yes" else "no"
  }

  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    p.contramap(_.value)
}
