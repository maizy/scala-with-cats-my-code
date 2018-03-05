package space.maizy.scalawithcats.ch1

/**
  * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
  * See LICENSE.txt for details.
  */

import cats._
import cats.syntax.contravariant._

object ShapeShowInstances {
  implicit val shapeShow: Show[Shape] =
    Show.show(value => s"Shape: volume=${value.volume.formatted("%.1f")}")

  implicit val squreShow: Show[Square] =
    Show.show(value => s"Square: side=${value.side}, volume=${value.volume.formatted("%.1f")}")

  implicit val showCircle: Show[Circle] = shapeShow.contramap(x => x: Shape)
}
