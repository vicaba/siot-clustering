package util
import breeze.linalg.Vector

import scala.math._
import scala.util.Random

object Generator {

  def generateRandom2DPoints(center: Vector[Double],
                             radius: Double,
                             numberOfPoints: Int,
                             angle: Double): IndexedSeq[Vector[Double]] = {

    for (_ <- 1 to numberOfPoints) yield {
      // Random from [0, 1]
      val angle = 2 * Pi * Random.nextDouble()
      val r     = radius * sqrt(Random.nextDouble())
      val x     = r * cos(angle) + center(0)
      val y     = r * sin(angle) + center(1)
      Vector[Double](x, y)
    }
  }
}
