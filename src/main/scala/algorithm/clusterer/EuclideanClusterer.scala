package algorithm.clusterer
import java.util.UUID

import algorithm.ClustererSettings
import algorithm.clusterer.clusterlimitheuristic.{ClusterLimitHeuristic, MaxEnergyHeuristic}
import algorithm.clusterer.elementlocatorheuristic.{ElementLocatorHeuristic, HeuristicChain, HeuristicDecorator}
import algorithm.util.ClusteringOrder
import eventmanager.EventManager
import metrics.Metric
import types.DataTypeMetadata.SyntheticDataType
import types.Type
import types.immutable.Point
import types.ops.MirrorImage
import types.mutable.Cluster

import scala.annotation.tailrec
import scala.collection.immutable.LinearSeq
import scala.collection.mutable
import scala.util.Random

object EuclideanClusterer {

  /**
    * Creates clusters by combining other clusters.
    *
    * @param iterations the max number of iterations that combine clusters
    * @param centroid the centroid of all clusters/all members in the dataset
    * @param freeClusters the remaining free clusters to assign to an upper level cluster
    * @param heuristic the heuristic used to calculate the distance between clusters
    * @param membersPerCluster the number of members per upper clusters
    * @param clusters  the clusters already formed. It is an accumulative variable
    * @param untilHeuristic if set, the heuristic that indicates when a given cluster is "full"
    * @return higher level clusters
    */
  @tailrec
  def clustersToClusters(iterations: Int,
                         centroid: SyntheticDataType,
                         freeClusters: IndexedSeq[Cluster],
                         heuristic: ElementLocatorHeuristic,
                         membersPerCluster: Int,
                         clusters: IndexedSeq[Cluster] = IndexedSeq(),
                         untilHeuristic: Option[ClusterLimitHeuristic] = None): IndexedSeq[Cluster] = {

    if (iterations > 0 && freeClusters.nonEmpty) {
      val head = freeClusters.head
      val c = Cluster(head.id + 1, UUID.randomUUID().toString, Set(head), head.hierarchyLevel + 1, None)(
        head.dataTypeMetadata)
      val tail = freeClusters.tail
      if (untilHeuristic.nonEmpty) {
        val (cluster, remainingClusters) =
          clustersToClusterUntilHeuristic(c, centroid, tail, heuristic, untilHeuristic.get)
        clustersToClusters(iterations - 1,
                           centroid,
                           remainingClusters,
                           heuristic,
                           membersPerCluster,
                           cluster +: clusters,
                           untilHeuristic)
      } else {
        val (cluster, remainingClusters) = clustersToClusterXTimes(c, centroid, tail, heuristic, membersPerCluster)
        clustersToClusters(iterations - 1,
                           centroid,
                           remainingClusters,
                           heuristic,
                           membersPerCluster,
                           cluster +: clusters)
      }

    } else {
      clusters
    }

  }

  def clustersToClusterUntilHeuristic(
      cluster: Cluster,
      centroid: SyntheticDataType,
      freeClusters: IndexedSeq[Cluster],
      heuristic: ElementLocatorHeuristic,
      clusterLimitHeuristic: ClusterLimitHeuristic = MaxEnergyHeuristic): (Cluster, IndexedSeq[Cluster]) = {
    @tailrec
    def _clustersToClusterUntilHeuristic(c: Cluster,
                                         freeClusters: IndexedSeq[Cluster],
                                         iterationCount: Int): (Cluster, IndexedSeq[Cluster]) = {

      if (freeClusters.nonEmpty) {

        val (clusterTry, _) = clustersToCluster(Type.deepCopy(c), centroid, freeClusters, heuristic)

        if (clusterLimitHeuristic(clusterBefore = c, clusterAfter = clusterTry)) {

          val (cluster, remainingClusters) = clustersToCluster(c, centroid, freeClusters, heuristic)

          _clustersToClusterUntilHeuristic(cluster, remainingClusters, iterationCount = 0)

        } else (c, freeClusters)

      } else (c, freeClusters)

    }

    _clustersToClusterUntilHeuristic(cluster, freeClusters, 0)
  }

  /**
    * Assigns clustersPerCluster number of clusters to a single cluster given freeClusters.
    * Usually clustersPerClusters <= count(freeClusters)
    *
    * @param c
    * @param centroid
    * @param freeClusters
    * @param heuristic
    * @param clustersPerCluster
    * @return
    */
  def clustersToClusterXTimes(c: Cluster,
                              centroid: SyntheticDataType,
                              freeClusters: IndexedSeq[Cluster],
                              heuristic: ElementLocatorHeuristic,
                              clustersPerCluster: Int = 1): (Cluster, IndexedSeq[Cluster]) = {

    @tailrec
    def _clustersToClusterXTimes(c: Cluster,
                                 freeClusters: IndexedSeq[Cluster],
                                 clustersPerCluster: Int): (Cluster, IndexedSeq[Cluster]) = {
      clustersPerCluster match {
        case 0 => (c, freeClusters)
        case _ =>
          val (cluster, remainingClusters) = clustersToCluster(c, centroid, freeClusters, heuristic)
          _clustersToClusterXTimes(cluster, remainingClusters, clustersPerCluster - 1)
      }
    }

    _clustersToClusterXTimes(c, freeClusters, clustersPerCluster - 1)

  }

  def clustersToCluster(c: Cluster,
                        centroid: SyntheticDataType,
                        freeClusters: IndexedSeq[Cluster],
                        heuristic: ElementLocatorHeuristic): (Cluster, IndexedSeq[Cluster]) = {
    val closestMirror = heuristic(c, centroid, freeClusters)
    if (closestMirror.isEmpty) (c, freeClusters)
    else {
      val mirrorIndex =
        freeClusters.indexWhere(_.id == closestMirror.head._2.id)
      val mirror            = freeClusters(mirrorIndex)
      val remainingClusters = freeClusters.patch(mirrorIndex, IndexedSeq(), 1)
      (c += mirror, remainingClusters)
    }
  }

  /**
    * Assigns freeClusters to fixedClusters.
    * This mutates fixedClusters and freeClusters!
    *
    * @param centroid
    * @param fixedClusters mutable
    * @param freeClusters mutable
    * @param heuristic
    * @return
    */
  @tailrec
  def clustersToFixedClusters(fixedClusters: IndexedSeq[Cluster],
                              freeClusters: IndexedSeq[Cluster],
                              centroid: SyntheticDataType,
                              heuristic: ElementLocatorHeuristic): IndexedSeq[Cluster] = {

    if (freeClusters.nonEmpty) {

      val thisFreeCluster = freeClusters.head

      val bestClusterToAssign = fixedClusters.minBy { fixedCluster =>
        heuristic(fixedCluster, centroid, IndexedSeq(thisFreeCluster)).head._1
      }

      def fittest(c: Cluster): Double = heuristic(c, centroid, IndexedSeq(thisFreeCluster)).head._1

      val (_, __bestClusterToAssign) = Cluster
        .traverseAndFindFittest(bestClusterToAssign, fittest _)

      __bestClusterToAssign += thisFreeCluster

      clustersToFixedClusters(fixedClusters, (freeClusters.toSet - thisFreeCluster).toIndexedSeq, centroid, heuristic)

    } else fixedClusters

  }

  /**
    * Given clusters, creates stopAtKClusters top-level clusters
    *
    * @param stopAtKClusters the number of top-level clusters to create
    * @param stopAtIterationCount
    * @param clusters the clusters to group
    * @param heuristic the heuristic to use to group clusters
    * @param startHeuristic if not empty, the heuristic to use to group the first level of clusters
    * @return
    */
  def cluster(stopAtKClusters: Int,
              stopAtIterationCount: Int,
              clusters: Seq[Cluster],
              heuristic: ElementLocatorHeuristic,
              startHeuristic: Option[ClusterLimitHeuristic] = None): LinearSeq[Cluster] = {

    if (clusters.isEmpty) return Nil
    if (stopAtKClusters == 1)
      return List(Cluster(1, "1", new mutable.HashSet[Cluster]() ++= clusters, 1, None)(clusters.head.dataTypeMetadata))

    val points   = clusters.flatMap(_.points)
    val centroid = Type.centroidOf(points)

    var iterations                     = 0
    var kClusters                      = clusters.size
    var _clusters: IndexedSeq[Cluster] = clusters.toIndexedSeq

    EventManager.singleton.publish("clusters", _clusters.toList)

    if (startHeuristic.nonEmpty) {
      _clusters =
        clustersToClusters(Int.MaxValue, centroid, _clusters, heuristic, Int.MaxValue, untilHeuristic = startHeuristic)
    }

    val clusteringOrder = ClusteringOrder(_clusters.size, stopAtKClusters)

    def hasReachedMaxAllowedIterations: Boolean                = iterations == stopAtIterationCount
    def hasReachedMaxIterationsBasedOnClusteringOrder: Boolean = iterations == clusteringOrder.order.size
    def minimumClustersReached: Boolean                        = kClusters <= stopAtKClusters

    while (!hasReachedMaxAllowedIterations &&
           !hasReachedMaxIterationsBasedOnClusteringOrder &&
           !minimumClustersReached) {

      val membersPerCluster = clusteringOrder.order(iterations)

      val clusteringIterations =
        if (iterations == 0)
          (_clusters.size - clusteringOrder.outliers) / membersPerCluster
        else _clusters.size / membersPerCluster

      _clusters =
        clustersToClusters(iterations = clusteringIterations, centroid, _clusters, heuristic, membersPerCluster)
      iterations = iterations + 1
      kClusters = _clusters.size

      EventManager.singleton.publish("clusters", _clusters.toList)

    }

    val outliers = (Cluster.flatten(clusters) -- Cluster.flatten(_clusters)).map(Point.toCluster)

    val finalClusters =
      clustersToFixedClusters(Type.deepCopy(_clusters).asInstanceOf[Traversable[Cluster]].toIndexedSeq,
                              outliers.toIndexedSeq,
                              centroid,
                              heuristic)

    if (outliers.nonEmpty) EventManager.singleton.publish("clusters", finalClusters.toList)

    finalClusters.toList

  }

  /**
    * Clusters maxIterations times and outputs the best solution
    *
    * @param clusters the clusters to group
    * @param metricToOptimize the metric that the grouping optimizes
    * @param clusterer the algorithm that clusters
    * @param maxIterations the maximum number of iterations to try to optimize metricToOptimize
    * @return the best solution found
    */
  def metricReductionCluster(clusters: LinearSeq[Cluster],
                             metricToOptimize: Metric,
                             clusterer: LinearSeq[Cluster] => LinearSeq[Cluster],
                             maxIterations: Int): LinearSeq[Cluster] = {
    var best: LinearSeq[Cluster] = null

    for (i <- 0 until maxIterations) {
      val result          = clusterer(Random.shuffle(clusters))
      val aggregateMetric = metricToOptimize.aggregateOf(result)
      val maxMetric       = metricToOptimize(result.maxBy(metricToOptimize(_)))
      if (i == 0) best = result
      else {
        val isCurrentAggregateMetricLowerThanBest = aggregateMetric <= metricToOptimize.aggregateOf(best)
        val isCurrentMaxMetricLowerThanBest       = maxMetric <= metricToOptimize(best.maxBy(metricToOptimize(_)))
        if (isCurrentAggregateMetricLowerThanBest && isCurrentMaxMetricLowerThanBest) best = result
      }
    }
    best
  }

  val mirrorElementLocator: ElementLocatorHeuristic =
    ElementLocatorHeuristic(MirrorImage.findClosestMirrors(_, _, _)(MirrorImage.MirroredCluster))

  val chain: HeuristicChain = HeuristicChain(HeuristicDecorator(mirrorElementLocator))

  def apply(settings: EuclideanClustererSettings): List[Cluster] = {

    val result = metricReductionCluster(
      settings.points.map(Point.toCluster).toList,
      Metric.par,
      cluster(settings.numberOfClusters, Int.MaxValue, _, chain, Option(MaxEnergyHeuristic)),
      settings.improveIterations
    ).toList

    result

  }

  def applyOnce(settings: EuclideanClustererSettings): List[Cluster] = {
    val result = cluster(settings.numberOfClusters, Int.MaxValue, settings.points.map(Point.toCluster).toList, chain)
    result.toList
  }

}
