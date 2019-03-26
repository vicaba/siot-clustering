package algorithm.clusterer
import java.util.UUID

import algorithm.clusterer.FlattenedEuclideanClusterer.{ClusteringOrder, Settings, centroidOf}
import breeze.linalg.{Axis, sum}
import config.Configuration
import eventmanager.EventManager
import metrics.{Metric, Par}
import types.DataTypeMetadata.SyntheticDataType
import types.Type
import types.immutable.Point
import types.ops.MirrorImage
import types.mutable.Cluster

import scala.annotation.tailrec
import scala.collection.immutable.LinearSeq
import scala.collection.mutable
import scala.util.Random
import types.ops.SetOps._

object EuclideanClusterer {

  /**
    * Settings for the euclidean clusterer
    *
    * @param numberOfClusters
    * @param points
    * @param metric
    * @param improveIterations
    */
  case class Settings(override val numberOfClusters: Int,
                      override val points: scala.Vector[Point],
                      override val metric: Metric,
                      override val improveIterations: Int = 1)
      extends algorithm.algorithms.ClustererSettings

  /**
    * Alias type that resumes the signature of heuristics
    */
  type ElementLocatorHeuristic = (Cluster, SyntheticDataType, IndexedSeq[Cluster]) => IndexedSeq[(Double, Cluster)]

  trait ClusterLimitHeuristic extends ((Cluster, Cluster) => Boolean) {
    def apply(clusterBefore: Cluster, clusterAfter: Cluster): Boolean
  }

  /**
    * Heuristic decorator that prunes some elements before returning the results of applying a heuristic
    *
    * @param heuristic the heuristic
    */
  case class HeuristicDecorator(heuristic: ElementLocatorHeuristic) extends ElementLocatorHeuristic {
    override def apply(v1: Cluster, v2: SyntheticDataType, v3: IndexedSeq[Cluster]): IndexedSeq[(Double, Cluster)] = {
      val clusters              = heuristic.apply(v1, v2, v3)
      val aprioriElementsToDrop = clusters.length / 2
      val elementsToDrop        = if (aprioriElementsToDrop < 1) aprioriElementsToDrop + 1 else aprioriElementsToDrop
      clusters.dropRight(aprioriElementsToDrop)
    }
  }

  /**
    * Chains a list of heuristics
    *
    * @param heuristics the list of heuristics
    */
  implicit class HeuristicChain(heuristics: List[ElementLocatorHeuristic]) extends ElementLocatorHeuristic {
    override def apply(v1: Cluster, v2: SyntheticDataType, v3: IndexedSeq[Cluster]): IndexedSeq[(Double, Cluster)] =
      heuristics.tail.foldLeft(heuristics.head.apply(v1, v2, v3)) {
        case (_clusters, _heuristic) =>
          _heuristic(v1, v2, _clusters.map(_._2))
      }
  }

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
                         untilHeuristic: Option[Cluster => Boolean] = None): IndexedSeq[Cluster] = {

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

  object MaxEnergyHeuristic extends ClusterLimitHeuristic {
    override def apply(clusterBefore: Cluster, clusterAfter: Cluster): Boolean = {
      val maxEnergyAllowed   = sum(clusterBefore.centroid) * (clusterBefore.points.size + 1)
      val potentialMaxEnergy = sum(clusterAfter.centroid) * clusterAfter.points.size
      potentialMaxEnergy <= maxEnergyAllowed
    }
  }

  def clustersToClusterUntilHeuristic(cluster: Cluster,
                                      centroid: SyntheticDataType,
                                      freeClusters: IndexedSeq[Cluster],
                                      heuristic: ElementLocatorHeuristic,
                                      clusterLimitHeuristic: ClusterLimitHeuristic = MaxEnergyHeuristic): (Cluster, IndexedSeq[Cluster]) = {
    @tailrec
    def _clustersToClusterUntilHeuristic(c: Cluster,
                                         freeClusters: IndexedSeq[Cluster],
                                         iterationCount: Int): (Cluster, IndexedSeq[Cluster]) = {

      val clusterCopy = Type.deepCopy(c)

      if (freeClusters.nonEmpty) {

        val (clusterTry, _) = clustersToCluster(clusterCopy, centroid, freeClusters, heuristic)

        if (clusterLimitHeuristic(clusterBefore = clusterCopy, clusterAfter = clusterTry)) {

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
              startHeuristic: Option[Cluster => Boolean] = None): LinearSeq[Cluster] = {

    val points   = clusters.flatMap(_.points)
    val centroid = centroidOf(points)

    var iterations                     = 0
    var kClusters                      = clusters.size
    var _clusters: IndexedSeq[Cluster] = clusters.toIndexedSeq

    if (clusters.isEmpty) return Nil
    if (stopAtKClusters == 1)
      return List(Cluster(1, "1", new mutable.HashSet[Cluster]() ++= clusters, 1, None)(clusters.head.dataTypeMetadata))

    EventManager.singleton.publish("clusters", _clusters.toList)

    if (startHeuristic.nonEmpty) {
      _clusters =
        clustersToClusters(Int.MaxValue, centroid, _clusters, heuristic, Int.MaxValue, untilHeuristic = startHeuristic)
    }

    val clusteringOrder = ClusteringOrder(_clusters.size, stopAtKClusters)

    def maxIterations: Boolean      = iterations == stopAtIterationCount
    def clusteringOrderEnd: Boolean = iterations == clusteringOrder.order.size
    def minClustersReached: Boolean = kClusters <= stopAtKClusters

    while (!maxIterations && !clusteringOrderEnd && !minClustersReached) {

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
        val currentAggregateMetricLowerThanBest = aggregateMetric <= metricToOptimize.aggregateOf(best)
        val currentMaxMetricLowerThanBest       = maxMetric <= metricToOptimize(best.maxBy(metricToOptimize(_)))
        if (currentAggregateMetricLowerThanBest && currentMaxMetricLowerThanBest) best = result
      }
    }
    best
  }

  val chain: HeuristicChain = List(
    HeuristicDecorator(MirrorImage.findClosestMirrors(_, _, _)(MirrorImage.MirroredCluster))) ::: Nil

  def apply(settings: Settings): List[Cluster] = {

    val startHeuristic: Cluster => Boolean = c => {
      val cluster  = sum(c.syntheticValue)
      val centroid = Configuration.ClusteringAlgorithm.leafEnergyConsumption(sum(centroidOf(settings.points)))
      val r        = cluster > centroid
      r
    }

    val result = metricReductionCluster(
      settings.points.map(Point.toCluster).toList,
      Metric.par,
      cluster(settings.numberOfClusters, Int.MaxValue, _, chain, Option(startHeuristic)),
      settings.improveIterations
    ).toList

    result

  }

  def applyOnce(settings: Settings): List[Cluster] = {
    val result = cluster(settings.numberOfClusters, Int.MaxValue, settings.points.map(Point.toCluster).toList, chain)
    result.toList
  }

}
