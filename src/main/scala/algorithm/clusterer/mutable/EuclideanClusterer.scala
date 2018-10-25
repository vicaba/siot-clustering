package algorithm.clusterer.mutable
import algorithm.clusterer.EuclideanClusterer.{ClusteringOrder, Heuristic, HeuristicChain, HeuristicDecorator, Settings, centroidOf}
import eventmanager.EventManager
import metrics.Metric
import types.Point
import types.mutable.Cluster
import types.Types.SyntheticDataType
import types.ops.MirrorImage

import scala.annotation.tailrec
import scala.collection.immutable.LinearSeq
import scala.util.Random
import types.ops.SetOps._

import scala.collection.mutable

object EuclideanClusterer {

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
      if (remainingClusters.isEmpty) cluster +: clusters
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
      (c ++= mirror.points, remainingClusters)
    }
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
    if (stopAtKClusters == 1) return List(new Cluster(1, "1",new mutable.HashSet[Cluster]().++=(clusters))(clusters.head.types))

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
      clustersToFixedClusters(centroid, _clusters, outliers.toIndexedSeq, heuristic)

    if (outliers.nonEmpty) EventManager.singleton.publish("clusters", finalClusters.toList)

    finalClusters.toList

  }

  @tailrec
  def clustersToFixedClusters(centroid: SyntheticDataType,
                              fixedClusters: IndexedSeq[Cluster],
                              freeClusters: IndexedSeq[Cluster],
                              heuristic: Heuristic): IndexedSeq[Cluster] = {
    if (freeClusters.nonEmpty) {
      var closestMirror: Cluster = null
      val bestClusterToAssign = fixedClusters.minBy { fixedCluster =>
        closestMirror = heuristic(fixedCluster, centroid, freeClusters).head._2
      }

      bestClusterToAssign += closestMirror

      clustersToFixedClusters(centroid,
                              (fixedClusters.toSet -/+ bestClusterToAssign).toIndexedSeq,
                              (freeClusters.toSet - closestMirror).toIndexedSeq,
                              heuristic)
    } else fixedClusters

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
    HeuristicDecorator(MirrorImage.findClosestMirrors(_, _, _)(MirrorImage.MirroredMutableCluster))) ::: Nil

  def apply(settings: Settings): List[Cluster] = {

    val clusteringOrder = ClusteringOrder(settings.points.size, settings.numberOfClusters)

    val result = metricReductionCluster(
      settings.points.map(Point.toMutableCluster).toList,
      Metric.par,
      cluster(settings.numberOfClusters, Int.MaxValue, _, chain, clusteringOrder),
      100
    ).toList

    result

  }

  def applyOnce(settings: Settings): List[Cluster] = {
    val clusteringOrder = ClusteringOrder(settings.points.size, settings.numberOfClusters)
    val result = cluster(settings.numberOfClusters,
                         Int.MaxValue,
                         settings.points.map(Point.toMutableCluster).toList,
                         chain,
                         clusteringOrder)
    result.toList
  }

}
