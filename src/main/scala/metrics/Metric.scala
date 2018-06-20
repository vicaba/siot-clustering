package metrics

import breeze.linalg._
import breeze.stats._

object Metric {
  def par: Par.type = Par
  def maxMin: MaxMin.type = MaxMin
}

trait Metric {
  def apply(e: DenseVector[Double]): Double
}

object MaxMin extends Metric {
  override def apply(e: DenseVector[Double]): Double = max(e) - min(e)
}

object Par extends Metric {
  override def apply(e: DenseVector[Double]): Double = max(e) /  mean(e)
}
