package space.maizy.scalawithcats.ch3

/**
  * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
  * See LICENSE.txt for details.
  */

trait Codec[A] {
  self =>
  def encode(v: A): String
  def decode(p: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] =
    new Codec[B] {
      override def encode(v: B): String = self.encode(enc(v))
      override def decode(p: String): B = dec(self.decode(p))
    }
}

object CodecOps {
  def encode[A](v: A)(implicit c: Codec[A]): String = c.encode(v)
  def decode[A](p: String)(implicit c: Codec[A]): A = c.decode(p)
}

object CodecInstances {

  implicit val stringCodec: Codec[String] = new Codec[String] {
    override def encode(v: String): String = v
    override def decode(p: String): String = p
  }

  implicit val intCodec: Codec[Int] = new Codec[Int] {
    override def encode(v: Int): String = v.toString
    override def decode(p: String): Int = p.toInt
  }

  implicit val booleadCodec: Codec[Boolean] = new Codec[Boolean] {
    override def encode(v: Boolean): String = v.toString
    override def decode(p: String): Boolean = Seq("true", "t", "1", "yes", "y") contains p
  }

  implicit val doubleCodecWithRound: Codec[Double] =
    intCodec.imap(_.toDouble, _.toInt)
}
