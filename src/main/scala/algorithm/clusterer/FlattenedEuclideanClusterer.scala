package algorithm.clusterer

import eventmanager.EventManager
import metrics.Metric
import types.Types.SyntheticDataType
import types.ops.MirrorImage
import types.{Cluster, Point, Types, Types2}
import utils.MathUtils

import scala.annotation.tailrec
import scala.collection.immutable.LinearSeq
import scala.util.Random
import scala.math._
import types.ops.SetOps._

object FlattenedEuclideanClusterer {

  case class Settings(override val numberOfClusters: Int,
                      override val points: scala.Vector[Point],
                      override val metric: Metric,
                      override val improveIterations: Int = 1)
      extends algorithm.algorithms.ClustererSettings

  type Heuristic = (Cluster, SyntheticDataType, IndexedSeq[Cluster]) => IndexedSeq[(Double, Cluster)]

  case class HeuristicDecorator(heuristic: Heuristic) extends Heuristic {
    override def apply(v1: Cluster, v2: SyntheticDataType, v3: IndexedSeq[Cluster]): IndexedSeq[(Double, Cluster)] = {
      val clusters              = heuristic.apply(v1, v2, v3)
      val aprioriElementsToDrop = clusters.length / 2
      val elementsToDrop        = if (aprioriElementsToDrop < 1) aprioriElementsToDrop + 1 else aprioriElementsToDrop
      clusters.dropRight(aprioriElementsToDrop)
    }
  }

  implicit class HeuristicChain(heuristics: List[Heuristic]) extends Heuristic {
    override def apply(v1: Cluster, v2: SyntheticDataType, v3: IndexedSeq[Cluster]): IndexedSeq[(Double, Cluster)] =
      heuristics.tail.foldLeft(heuristics.head.apply(v1, v2, v3)) {
        case (_clusters, _heuristic) =>
          _heuristic(v1, v2, _clusters.map(_._2))
      }
  }

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
      val clusteringOrder             = MathUtils.factorize(numberOfPoints / kClusters)
      val outliersFromClusteringOrder = numberOfPoints % kClusters

      ClusteringOrder(clusteringOrder, outliersFromClusteringOrder)
    }
  }

  def centroidOf[T <: Types.Type](points: Seq[T]): types.Types.SyntheticDataType =
    points.foldLeft(points.head.types.EmptySyntheticData()) {
      case (accum, p) =>
        accum + p.syntheticValue
    } / points.length.toDouble

  @tailrec
  def clustersToClusters(iterations: Int,
                         centroid: SyntheticDataType,
                         freeClusters: IndexedSeq[Cluster],
                         heuristic: Heuristic,
                         membersPerCluster: Int,
                         clusters: IndexedSeq[Cluster] = IndexedSeq()): IndexedSeq[Cluster] = {

    if (iterations > 0 && freeClusters.nonEmpty) {
      val c                            = freeClusters.head
      val tail                         = freeClusters.tail
      val (cluster, remainingClusters) = clustersToClusterXTimes(c, centroid, tail, heuristic, membersPerCluster)
      clustersToClusters(iterations - 1, centroid, remainingClusters, heuristic, membersPerCluster, cluster +: clusters)
    } else {
      clusters
    }


  }

  def clustersToClusterXTimes(c: Cluster,
                              centroid: SyntheticDataType,
                              freeClusters: IndexedSeq[Cluster],
                              heuristic: Heuristic,
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
                        heuristic: Heuristic): (Cluster, IndexedSeq[Cluster]) = {
    val closestMirror = heuristic(c, centroid, freeClusters)
    if (closestMirror.isEmpty) (c, freeClusters)
    else {
      val mirrorIndex =
        freeClusters.indexWhere(_.id == closestMirror.head._2.id)
      val mirror            = freeClusters(mirrorIndex)
      val remainingClusters = freeClusters.patch(mirrorIndex, IndexedSeq(), 1)
      val clusterPoints     = new scala.collection.mutable.HashSet[Types.Type]() ++= (c.points ++ mirror.points)
      (c.copy(points = clusterPoints), remainingClusters)
    }
  }

  def clustersToFixedClusters(centroid: SyntheticDataType,
                              fixedClusters: IndexedSeq[Cluster],
                              freeClusters: IndexedSeq[Cluster],
                              heuristic: Heuristic): IndexedSeq[Cluster] = {
    if (freeClusters.nonEmpty) {
      var closestMirror: Cluster = null
      val bestClusterToAssign = fixedClusters.minBy { fixedCluster =>
        val result = heuristic(fixedCluster, centroid, freeClusters).head
        closestMirror = result._2
        result._1
      }
      val newFixedCluster =
        bestClusterToAssign.copy(points = bestClusterToAssign.points ++ closestMirror.points)
      clustersToFixedClusters(centroid,
        (fixedClusters.toSet -/+ newFixedCluster).toIndexedSeq,
        (freeClusters.toSet - closestMirror).toIndexedSeq,
        heuristic)
    } else fixedClusters

  }

  def cluster(stopAtKClusters: Int,
              stopAtIterationCount: Int,
              clusters: Seq[Cluster],
              heuristic: Heuristic,
              clusteringOrder: ClusteringOrder): LinearSeq[Cluster] = {

    val points   = clusters.flatMap(_.points)
    val centroid = centroidOf(points)

    var iterations                     = 0
    var kClusters                      = clusters.size
    var _clusters: IndexedSeq[Cluster] = clusters.toIndexedSeq

    if (clusters.isEmpty) return Nil
    if (stopAtKClusters == 1) return List(Cluster(1, "1", points.toSet, 0, None)(clusters.head.types))

    EventManager.singleton.publish("clusters", _clusters.toList)

    def maxIterations: Boolean      = iterations == stopAtIterationCount
    def clusteringOrderEnd: Boolean = iterations == clusteringOrder.order.size
    def minClustersReached: Boolean = kClusters <= stopAtKClusters

    while (!maxIterations && !clusteringOrderEnd && !minClustersReached) {

      val membersPerCluster = clusteringOrder.order(iterations)

      _clusters = clustersToClusters(iterations = _clusters.size / membersPerCluster,
                                     centroid,
                                     _clusters,
                                     heuristic,
                                     membersPerCluster)
      iterations = iterations + 1
      kClusters = _clusters.size

      EventManager.singleton.publish("clusters", _clusters.toList)

    }

    val outliers = clusters.flatMap(_.points).toSet -- _clusters.flatMap(_.points).toSet

    val finalClusters =
      clustersToFixedClusters(centroid, _clusters, outliers.map(Types.Type.toCluster).toIndexedSeq, heuristic)

    if (outliers.nonEmpty) EventManager.singleton.publish("clusters", finalClusters.toList)

    finalClusters.toList

  }

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
        if (aggregateMetric <= metricToOptimize.aggregateOf(best) && maxMetric <= metricToOptimize(
              best.maxBy(metricToOptimize(_)))) best = result
      }
    }
    best
  }

  val chain: HeuristicChain = List(
    HeuristicDecorator(MirrorImage.findClosestMirrors(_, _, _)(MirrorImage.MirroredCluster))) ::: Nil

  def apply(settings: Settings): List[Cluster] = {

    val clusteringOrder = ClusteringOrder(settings.points.size, settings.numberOfClusters)

    val result = metricReductionCluster(
      settings.points.map(Point.toCluster).toList,
      Metric.par,
      cluster(settings.numberOfClusters, Int.MaxValue, _, chain, clusteringOrder),
      settings.improveIterations
    ).toList

    result

  }

  def applyOnce(settings: Settings): List[Cluster] = {
    val clusteringOrder = ClusteringOrder(settings.points.size, settings.numberOfClusters)
    val result = cluster(settings.numberOfClusters,
                         Int.MaxValue,
                         settings.points.map(Point.toCluster).toList,
                         chain,
                         clusteringOrder)
    result.toList
  }

}
