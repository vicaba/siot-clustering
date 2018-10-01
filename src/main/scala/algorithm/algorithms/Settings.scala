package algorithm.algorithms
import metrics.Metric

trait Settings {

  val numberOfClusters: Int

  val metric: Metric

}
