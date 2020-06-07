package Lab3

import scala.math.Pi


abstract class calcCircleParams[T] {
  def calcL(r: T): T
  def calcS(r: T): T
}

object calcCircleParams {
  implicit object double extends calcCircleParams[Double] {
    override def calcL(r: Double): Double = {
      2 * Pi * r
    }

    override def calcS(r: Double): Double = {
      Pi * r * r
    }
  }

  implicit object float extends calcCircleParams[Float] {
    override def calcL(r: Float): Float = {
      (2 * Pi * r).toFloat
    }

    override def calcS(r: Float): Float = {
      (Pi * r * r).toFloat
    }
  }
}
