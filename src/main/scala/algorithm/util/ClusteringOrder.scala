package algorithm.util
import utils.MathUtils

/**
  * This type represents the number of points that belong to each cluster per iteration in order to match
  * the generation of K clusters.
  *
  * @param order A list containing the number of points that belong to each cluster per iteration
  * @param outliers The points that do not fit in the clustering order to match K clusters
  */
case class ClusteringOrder(order: List[Int], outliers: Int)

object ClusteringOrder {
  def apply(numberOfPoints: Int, kClusters: Int): ClusteringOrder = {
    val clusteringOrder             = MathUtils.factorize(numberOfPoints / kClusters).sorted.reverse
    val outliersFromClusteringOrder = numberOfPoints % kClusters

    ClusteringOrder(clusteringOrder, outliersFromClusteringOrder)
  }
}
