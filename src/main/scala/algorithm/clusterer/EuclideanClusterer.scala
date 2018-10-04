package algorithm.clusterer
import breeze.linalg.DenseVector
import eventmanager.EventManager
import metrics.Metric
import types.Types.SyntheticDataType
import types.ops.MirrorImage
import types.{Cluster, Point, Types, Types2}

import scala.annotation.tailrec
import scala.collection.immutable.LinearSeq
import scala.util.Random

object EuclideanClusterer {

  case class Settings(override val numberOfClusters: Int,
                      points: scala.Vector[Point],
                      override val metric: Metric,
                      times: Int = 1)
      extends algorithm.algorithms.Settings

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

  def centroidOf[T <: Types.Type](points: Seq[T]): types.Types.SyntheticDataType =
    points.foldLeft(points.head.types.EmptySyntheticData()) {
      case (accum, p) =>
        accum + p.syntheticValue
    } / points.length.toDouble

  @tailrec
  def clustersToClusters(centroid: SyntheticDataType,
                         freeClusters: IndexedSeq[Cluster],
                         heuristic: Heuristic,
                         clusters: LinearSeq[Cluster] = LinearSeq()): LinearSeq[Cluster] = {
    freeClusters match {
      case c +: tail =>
        val closestMirror = heuristic(c, centroid, tail)
        if (closestMirror.isEmpty) c +: clusters
        else {
          val mirrorIndex =
            tail.indexWhere(_.id == closestMirror.head._2.id)
          val mirror             = tail(mirrorIndex)
          val remainingClusters  = tail.patch(mirrorIndex, IndexedSeq(), 1)
          val lastCreatedCluster = clusters.headOption.map(_.id).getOrElse(1)
          val clusterPoints      = c.points ++ mirror.points
          val cluster =
            Cluster(lastCreatedCluster + 1, s"${lastCreatedCluster + 1}", clusterPoints)(c.types)
          clustersToClusters(centroid, remainingClusters, heuristic, cluster +: clusters)
        }
      case IndexedSeq() => clusters
    }
  }

  def cluster(stopAtKClusters: Int,
              stopAtIterationCount: Int,
              clusters: LinearSeq[Cluster],
              heuristic: Heuristic): LinearSeq[Cluster] = {

    val points   = clusters.flatMap(_.points)
    val centroid = centroidOf(points)

    var iterations              = 0
    var kClusters               = clusters.size
    var _clusters: Seq[Cluster] = clusters

    if (clusters.isEmpty) return Nil
    if (stopAtKClusters == 1) return List(Cluster(1, "1", points.toSet)(clusters.head.types))

    EventManager.singleton.publish("clusters", _clusters.toList)

    while (iterations < stopAtIterationCount && kClusters > stopAtKClusters) {

      _clusters = clustersToClusters(centroid, _clusters.toVector, heuristic, Nil)
      iterations = iterations + 1
      kClusters = _clusters.size

      EventManager.singleton.publish("clusters", _clusters.toList)

    }
    _clusters.toList

  }

  def metricReductionCluster(clusters: LinearSeq[Cluster],
                             metricToOptimize: Metric,
                             clusterer: LinearSeq[Cluster] => LinearSeq[Cluster],
                             maxIterations: Int): LinearSeq[Cluster] = {
    var best: LinearSeq[Cluster] = null

    for (i <- 0 to maxIterations) {
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
    metricReductionCluster(settings.points.map { point =>
      Cluster(point.id, point.id.toString, Set(point))(point.types)
    }.toList, Metric.par, cluster(settings.numberOfClusters, Int.MaxValue, _, chain), 50).toList
  }

}
