package Lab3

import scala.math.sqrt

abstract class Checking[T] {
  def checkInside(circle1: Circle[T], circle2: Circle[T]): Boolean

  def check(x1: Double, y1: Double, x2: Double, y2: Double, r1: Double, r2: Double): Boolean = {
    sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)) + r2 <= r1
  }

}

object Checking {
  implicit object int extends Checking[Int] {
    override def checkInside(circle1: Circle[Int], circle2: Circle[Int]): Boolean = {
      check(circle1.x, circle1.y, circle2.x, circle2.y, circle1.r, circle2.r)
    }
  }

  implicit object double extends Checking[Double] {
    override def checkInside(circle1: Circle[Double], circle2: Circle[Double]): Boolean = {
      check(circle1.x, circle1.y, circle2.x, circle2.y, circle1.r, circle2.r)
    }
  }

  implicit object float extends Checking[Float] {
    override def checkInside(circle1: Circle[Float], circle2: Circle[Float]): Boolean = {
      check(circle1.x, circle1.y, circle2.x, circle2.y, circle1.r, circle2.r)
    }
  }

  implicit object long extends Checking[Long] {
    override def checkInside(circle1: Circle[Long], circle2: Circle[Long]): Boolean = {
      check(circle1.x, circle1.y, circle2.x, circle2.y, circle1.r, circle2.r)
    }
  }

  implicit object short extends Checking[Short] {
    override def checkInside(circle1: Circle[Short], circle2: Circle[Short]): Boolean = {
      check(circle1.x, circle1.y, circle2.x, circle2.y, circle1.r, circle2.r)
    }
  }

  implicit object byte extends Checking[Byte] {
    override def checkInside(circle1: Circle[Byte], circle2: Circle[Byte]): Boolean = {
      check(circle1.x, circle1.y, circle2.x, circle2.y, circle1.r, circle2.r)
    }
  }
}
