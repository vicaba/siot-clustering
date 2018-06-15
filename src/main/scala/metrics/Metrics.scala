package metrics

import breeze.linalg.{norm, _}
import breeze.math._
import breeze.numerics._
import cluster.Types

object Metrics {

  def par(e1: DenseVector[Double], e2: DenseVector[Double]): Double = {
    val sum = e1 + e2
    max(sum) - min(sum)
  }

}
