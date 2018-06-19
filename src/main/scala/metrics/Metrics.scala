package metrics

import breeze.linalg._

object Metrics {
  def par(e1: DenseVector[Double], e2: DenseVector[Double]): Double = Par(e1, e2)
}

trait Metric {
  def apply(e1: DenseVector[Double], e2: DenseVector[Double]): Double
}

object Par extends Metric {
  override def apply(e1: DenseVector[Double], e2: DenseVector[Double]): Double = {
    val sum = e1 + e2
    max(sum) - min(sum)
  }
}
