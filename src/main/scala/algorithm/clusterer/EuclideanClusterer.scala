package algorithm.clusterer
import java.util.UUID

import algorithm.ClustererSettings
import algorithm.clusterer.keepclusteringheuristic.{KeepClusteringHeuristic, MaxElementsHeuristic, MaxEnergyHeuristic}
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
    * @param elementLocatorHeuristic the heuristic used to calculate the distance between clusters
    * @param membersPerCluster the number of members per upper clusters
    * @param keepClusteringHeuristic if set, the heuristic that indicates when a given cluster is "full"
    * @return higher level clusters
    */
  def clustersToClusters(iterations: Int,
                         centroid: SyntheticDataType,
                         freeClusters: IndexedSeq[Cluster],
                         elementLocatorHeuristic: ElementLocatorHeuristic,
                         membersPerCluster: Int,
                         keepClusteringHeuristic: Option[KeepClusteringHeuristic] = None): IndexedSeq[Cluster] = {

    @tailrec
    def clusterToClustersInternal(iterations: Int,
                                  freeClusters: IndexedSeq[Cluster],
                                  accum: IndexedSeq[Cluster] = IndexedSeq()): IndexedSeq[Cluster] =
      if (iterations > 0 && freeClusters.nonEmpty) {

        val head = freeClusters.head
        val c = Cluster(head.id + 1, UUID.randomUUID().toString, Set(head), head.hierarchyLevel + 1, None)(
          head.dataTypeMetadata)
        val tail = freeClusters.tail

        if (keepClusteringHeuristic.nonEmpty) {

          val (cluster, remainingClusters) =
            applyHeuristic(c, centroid, tail, elementLocatorHeuristic, keepClusteringHeuristic.get)
          clusterToClustersInternal(iterations - 1, remainingClusters, cluster +: accum)

        } else {

          val lh                           = new MaxElementsHeuristic(membersPerCluster)
          val (cluster, remainingClusters) = applyHeuristic(c, centroid, tail, elementLocatorHeuristic, lh)

          clusterToClustersInternal(iterations - 1, remainingClusters, cluster +: accum)

        }

      } else {
        accum
      }

    clusterToClustersInternal(iterations, freeClusters)

  }

  def applyHeuristic(c: Cluster,
                     centroid: SyntheticDataType,
                     freeClusters: IndexedSeq[Cluster],
                     elementLocatorHeuristic: ElementLocatorHeuristic,
                     keepClusteringHeuristic: KeepClusteringHeuristic): (Cluster, IndexedSeq[Cluster]) = {

    @tailrec
    def applyHeuristicInternal(c: Cluster,
                               centroid: SyntheticDataType,
                               freeClusters: IndexedSeq[Cluster]): (Cluster, IndexedSeq[Cluster]) = {

      val eOpt = elementLocatorHeuristic(c, centroid, freeClusters).headOption

      if (eOpt.nonEmpty) {

        val e     = eOpt.get._2
        val continue = keepClusteringHeuristic(c, e)

        if (continue) {

          val eIndex =
            freeClusters.indexWhere(_.id == e.id)

          applyHeuristicInternal(c + e, centroid, freeClusters.patch(eIndex, IndexedSeq(), 1))

        } else (c, freeClusters)

      } else (c, freeClusters)
    }

    applyHeuristicInternal(c, centroid, freeClusters)

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
              startHeuristic: Option[KeepClusteringHeuristic] = None): LinearSeq[Cluster] = {

    if (clusters.isEmpty) return Nil
    if (stopAtKClusters == 1)
      return List(Cluster(1, "1", new mutable.HashSet[Cluster]() ++= clusters, 1, None)(clusters.head.dataTypeMetadata))

    val points                         = clusters.flatMap(_.points)
    val centroid                       = Type.centroidOf(points)
    var _clusters: IndexedSeq[Cluster] = clusters.toIndexedSeq

    EventManager.singleton.publish("clusters", _clusters.toList)

    if (startHeuristic.nonEmpty) {
      _clusters = clustersToClusters(Int.MaxValue,
                                     centroid,
                                     _clusters,
                                     heuristic,
                                     Int.MaxValue,
                                     keepClusteringHeuristic = startHeuristic)
    }

    var iterations      = 0
    var kClusters       = _clusters.size
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

    val res = finalClusters.toList

    res

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
      cluster(settings.numberOfClusters, Int.MaxValue, _, chain, None),
      settings.improveIterations
    ).toList

    result

  }

  def applyOnce(settings: EuclideanClustererSettings): List[Cluster] = {
    val result = cluster(settings.numberOfClusters, Int.MaxValue, settings.points.map(Point.toCluster).toList, chain)
    result.toList
  }

}
