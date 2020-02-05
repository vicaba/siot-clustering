package algorithm.clusterer

import algorithm.ClustererSettings
import collection.shuffler.{Random, Shuffler}
import metrics.Metric
import types.clusterer.immutable.Point

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
                                      override val improveIterations: Int = 1,
                                      override val shuffler: Shuffler = Random(scala.util.Random))
    extends ClustererSettings
