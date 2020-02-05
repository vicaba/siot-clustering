package algorithm

import collection.shuffler.Shuffler
import metrics.Metric
import types.clusterer.immutable.Point
trait Settings {

  val numberOfClusters: Int

  val metric: Metric

  val improveIterations: Int = 1

}

trait ClustererSettings extends Settings {

  val points: scala.Vector[Point]

  val shuffler: Shuffler

}
