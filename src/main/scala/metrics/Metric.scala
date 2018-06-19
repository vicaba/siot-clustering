package metrics

import breeze.linalg._
import breeze.stats._

object Metric {
  def par: Par.type = Par
  def maxMin: MaxMin.type = MaxMin
}

trait Metric {
  def apply(e: DenseVector[Double]): Double
  def apply(e1: DenseVector[Double], e2: DenseVector[Double]): Double
}

object MaxMin extends Metric {
  override def apply(e: DenseVector[Double]): Double = max(e) - min(e)
  override def apply(e1: DenseVector[Double], e2: DenseVector[Double]): Double = apply(e1 + e2)
}

object Par extends Metric {
  override def apply(e: DenseVector[Double]): Double = mean(e) / max(e)
  override def apply(e1: DenseVector[Double], e2: DenseVector[Double]): Double = apply(e1 + e2)
}
