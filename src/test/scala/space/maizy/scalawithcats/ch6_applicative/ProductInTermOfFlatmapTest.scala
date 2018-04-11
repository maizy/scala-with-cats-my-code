package space.maizy.scalawithcats.ch6_applicative

/**
 * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
 * See LICENSE.txt for details.
 */

import space.maizy.scalawithcats.BaseSpec
import space.maizy.scalawithcats.сh6_applicative.ProductInTermOfFlatmap

class ProductInTermOfFlatmapTest extends BaseSpec {
  "ProductInTermOfFlatmap" should "works" in {
    import cats.Id
    import cats.instances.list._
    import cats.syntax.applicative._
    ProductInTermOfFlatmap.product(5.pure[Id], 6.pure[Id]) shouldBe ((5, 6))
    ProductInTermOfFlatmap.product(5: Id[Int], 6: Id[Int]) shouldBe ((5, 6))
    ProductInTermOfFlatmap.product(List(1, 2), List(3, 4)) shouldBe List((1, 3), (1, 4), (2, 3), (2, 4))
  }
}
