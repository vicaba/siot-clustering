package metrics

import breeze.linalg.{DenseMatrix, DenseVector, max, min}
import cluster.Types

object Metrics {

  def par(e1: DenseVector[Int], e2: DenseVector[Int]): Double = {
    val sum = e1 + e2
    max(sum) - min(sum)
  }

}
