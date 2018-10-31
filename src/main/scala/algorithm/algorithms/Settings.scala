package algorithm.algorithms
import metrics.Metric

trait Settings {

  val numberOfClusters: Int

  val metric: Metric

  val improveIterations: Int = 1

}
