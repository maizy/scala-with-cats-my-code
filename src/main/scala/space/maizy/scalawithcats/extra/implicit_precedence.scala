package space.maizy.scalawithcats.extra

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */


trait MyTypeClass[T] {
  def label(v: T): String
}

object StaticHelper {
  def getLabel[T](v: T)(implicit ev: MyTypeClass[T]): String = ev.label(v)
}

object MyTypeClass {
  implicit val forInt: MyTypeClass[Int] = new MyTypeClass[Int] {
    override def label(v: Int): String = "int"
  }
}

class MySimpleObject(val l: String)

object MySimpleObject {
  implicit val label: MyTypeClass[MySimpleObject] = new MyTypeClass[MySimpleObject] {
    override def label(v: MySimpleObject): String = s"SimpleObject: ${v.l}"
  }
}

class MyCustomObjectParent(val l: String)

object MyCustomObjectParent {
  implicit val label: MyTypeClass[MyCustomObjectParent] = new MyTypeClass[MyCustomObjectParent] {
    override def label(v: MyCustomObjectParent): String = s"Object: ${v.l}"
  }
}

class MyCustomObject(val ll: String) extends MyCustomObjectParent(s"$ll-$ll")

class MyCustomObjectOverwrited(val ll: String) extends MyCustomObjectParent(s"$ll-$ll-over")

object MyCustomObjectOverwrited {
  implicit val label: MyTypeClass[MyCustomObjectOverwrited] = new MyTypeClass[MyCustomObjectOverwrited] {
    override def label(v: MyCustomObjectOverwrited): String = s"overwrite: ${v.l}"
  }
}
