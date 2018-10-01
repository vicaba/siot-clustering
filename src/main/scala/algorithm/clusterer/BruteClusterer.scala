package algorithm.clusterer

import eventmanager.EventManager
import metrics.Metric
import types._

import scala.annotation.tailrec
import scala.util.Random

object BruteClusterer {

  case class Settings(numberOfClusters: Int, points: scala.Vector[Point], metric: Metric, times: Int = 1)

  def apply(settings: Settings): List[Cluster] = {

    def aggregateErrorOf(clusters: List[Cluster]): Double = settings.metric.aggregateOf(clusters)

    (for (i <- 0 until settings.times)
      yield {
        val result = runOnce(settings.numberOfClusters, settings.points, settings.metric)
        EventManager.singleton.publish("iteration", (settings, result))
        result
      }).minBy(cl => aggregateErrorOf(cl))
  }

  def distanceTo(cluster: Cluster, averagePointsPerCluster: Int): Double =
    Metric.par(cluster)

  private def runOnce(numberOfClusters: Int, points: scala.Vector[Point], metric: Metric): List[Cluster] = {

    implicit def clusterToTuple(c: Cluster): (Int, Cluster) = c.id -> c

    def collectPoints(clusters: Iterable[Cluster]): scala.Vector[Point] =
      clusters
        .foldLeft(clusters.head.points) {
          case (accum, cluster) => accum ++ cluster.points
        }
        .toVector

    case class BestConfiguration(clusters: Iterable[Cluster], aggregatedMetric: Double)

    object BestConfiguration {
      def empty(metric: Metric): BestConfiguration = BestConfiguration(Nil, aggregatedMetric = metric.Highest)
    }

    @tailrec
    def assignToClusters(clusters: Map[Int, Cluster],
                         remainingPoints: scala.Vector[Point],
                         distanceF: Cluster => Double,
                         metrics: List[Double],
                         bestConfiguration: BestConfiguration): List[Cluster] =
      remainingPoints match {
        case p +: tail =>
          val bestClusterToAssignLocally = clusters.values.minBy { cluster =>
            p.assignedToCluster
              .map { cId =>
                if (cId == cluster.id) distanceF(cluster) else distanceF(cluster + p)
              }
              .getOrElse(distanceF(cluster + p))
          }

          val bestClusterToAssignGlobally = clusters.values.minBy { cluster =>
            p.assignedToCluster
              .map { cId =>
                if (cId == cluster.id) metric.aggregateOf(clusters.values)
                else metric.aggregateOf((clusters + (cluster + p)).values)
              }
              .getOrElse(metric.aggregateOf((clusters + (cluster + p)).values))
          }

          val bestClusterToAssign = bestClusterToAssignGlobally

          val newClusters =
            if (p.assignedToCluster.isDefined)
              if (p.assignedToCluster.get == bestClusterToAssign.id)
                clusters
              else
                clusters ++ Map(bestClusterToAssign + p, clusters(p.assignedToCluster.get) - p)
            else
              clusters + (bestClusterToAssign + p)

          val oldMetric = metric.aggregateOf(clusters.values.toList)
          val newMetric = metric.aggregateOf(newClusters.values.toList)

          val nextClusters =
            if ((newMetric <= oldMetric) || collectPoints(clusters.values).size != points.size) newClusters
            else clusters

          assignToClusters(
            nextClusters,
            tail,
            distanceF,
            metrics,
            bestConfiguration
          )

        case IndexedSeq() =>
          val currentMetric = metric.aggregateOf(clusters.values.toList)
          val _bestConfiguration =
            if (currentMetric < bestConfiguration.aggregatedMetric)
              BestConfiguration(clusters.values, currentMetric)
            else
              bestConfiguration

          if (metrics.contains(currentMetric) && _bestConfiguration == bestConfiguration)
            bestConfiguration.clusters.toList
          else
            assignToClusters(clusters,
                             collectPoints(clusters.values),
                             distanceF,
                             metrics :+ currentMetric,
                             _bestConfiguration)
      }

    if (points.nonEmpty) {

      implicit val types: TypesT = points.head.types

      val averagePointsPerCluster = points.length / numberOfClusters

      val randomSamplePoints                = randomSample(numberOfClusters, points)
      val r                                 = new Random(System.currentTimeMillis)
      val shuffledPointsWithoutClusterSeeds = r.shuffle((points.toSet -- randomSamplePoints.toSet).toVector)

      val _clusters = randomSamplePoints.zipWithIndex.map {
        case (point, idx) =>
          idx -> Cluster(idx, idx.toString, Set(point.setCluster(idx)))
      }.toMap

      // First round without the points assigned to each cluster
      assignToClusters(_clusters,
                       shuffledPointsWithoutClusterSeeds,
                       distanceTo(_, averagePointsPerCluster),
                       metric.Highest :: Nil,
                       BestConfiguration.empty(metric))

    } else Nil

  }

  private def randomSample(take: Int, points: Seq[Point]): List[Point] = {
    val r = new Random(System.currentTimeMillis)
    r.shuffle(points).take(take).toList
  }

}
