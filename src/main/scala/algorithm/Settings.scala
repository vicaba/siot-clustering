package algorithm

import metrics.Metric
import types.immutable.Point
trait Settings {

  val numberOfClusters: Int

  val metric: Metric

  val improveIterations: Int = 1

}

trait ClustererSettings extends Settings {

  val points: scala.Vector[Point]

}
