package algorithm.clusterer

import algorithm.ClustererSettings
import metrics.Metric
import types.immutable.Point

/**
  * Settings for the euclidean clusterer
  *
  * @param numberOfClusters
  * @param points
  * @param metric
  * @param improveIterations
  */
case class EuclideanClustererSettings(override val numberOfClusters: Int,
                                      override val points: scala.Vector[Point],
                                      override val metric: Metric,
                                      override val improveIterations: Int = 1)
  extends ClustererSettings