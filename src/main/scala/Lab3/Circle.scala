package Lab3


class Circle[T: Numeric] (val x: T, val y: T, val r: T) {

  def checkInside(circle: Circle[T]) (implicit checking: Checking[T]): Boolean = {
    checking.checkInside(this, circle)
  }

  def L() (implicit calcCircleParams: calcCircleParams[T]): T = {
    calcCircleParams.calcL(r)
  }

  def S() (implicit calcCircleParams: calcCircleParams[T]): T = {
    calcCircleParams.calcS(r)
  }

  override def toString: String = {
    val sb = new StringBuilder
    sb.append("(" + x + ", " + y + ") r = " + r)
    sb.toString()
  }
}


object Lab3 {

  def main(args: Array[String]): Unit = {
    val circleInt1 = new Circle[Int](0, 0, 5)
    val circleInt2 = new Circle[Int](0, 0, 2)
    val circleInt3 = new Circle[Int](4, 4, 2)
    println(circleInt2 + " inside " + circleInt1 + " = " + circleInt1.checkInside(circleInt2))
    println(circleInt3 + " inside " + circleInt1 + " = " + circleInt1.checkInside(circleInt3))

    val circleDouble = new Circle[Double](12.5, 13.4, 1.5)
    println(circleDouble + " L = " + circleDouble.L + "; S = " + circleDouble.S())
  }
}