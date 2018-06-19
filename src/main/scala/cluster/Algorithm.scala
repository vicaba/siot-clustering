package cluster

import cluster.Types._
import metrics.Metric

import scala.annotation.tailrec
import scala.util.Random

class Algorithm {

  def distanceTo(point: Point, cluster: Cluster): Double =
    Metric.par(point.syntheticValue, cluster.syntheticCenter)


  def run(numberOfClusters: Int, points: scala.Vector[Point]) = {

    val clusters = randomSample(numberOfClusters, points).zipWithIndex.map { case (point, idx) =>
      idx -> Cluster(idx, idx.toString, Set(point.setCluster(idx)))
    }.toMap

    @tailrec
    def assignToClusters(clusters: Map[Int, Cluster], remainingPoints: scala.Vector[Point]): Map[Int, Cluster] =
      remainingPoints match {
        case p +: tail =>

          val bestCluster = clusters.values.minBy { cluster =>
            if (p.isAssignedToCluster) {
              distanceTo(p, cluster - p)
            } else {
              distanceTo(p, cluster)
            }
          }

          assignToClusters(clusters + (bestCluster.id -> (bestCluster + p)), tail)
        case IndexedSeq() => clusters
      }

    assignToClusters(clusters, points)

  }


  private def randomSample(take: Int, points: Seq[Point]): List[Point] = {
    val r = new Random(100)
    r.shuffle(points).take(take).toList
  }


}
