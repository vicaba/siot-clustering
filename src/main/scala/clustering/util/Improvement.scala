package clustering.util

class Improvement(val initialMetric: Double) {

  def apply(currentMetric: Double): Double = 1 - currentMetric / initialMetric

}
