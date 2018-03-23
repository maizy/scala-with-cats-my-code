package space.maizy.scalawithcats.ch1_intro

/**
  * Copyright (c) Nikita Kovaliov, maizy.ru, 2018
  * See LICENSE.txt for details.
  */

class Shape(val volume: Double)

class Square(val side: Double) extends Shape(side * side)

class Circle(val r: Double) extends Shape(Math.pow(r, 2) * Math.PI)

