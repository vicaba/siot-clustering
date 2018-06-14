package cluster

import breeze.linalg._
import cluster.Types._

import scala.annotation.tailrec
import scala.util.Random

class Algorithm
{

  def distanceTo(p: Point, cluster: Cluster): Int =
  {
    max(p.syntheticValue) - min(p.syntheticValue)
  }

  def run(numberOfClusters: Int, points: scala.Vector[Point]) =
  {

    val clusters = randomSample(numberOfClusters, points).zipWithIndex.map { case (point, idx) =>
      idx -> Cluster(idx, idx.toString, Vector(point.setCluster(idx)))
    }.toMap

    @tailrec
    def assignToClusters(clusters: Map[Int, Cluster], remainingPoints: scala.Vector[Point]): Map[Int, Cluster] =
      remainingPoints match {
        case p +: tail =>


          assignToClusters(clusters, tail)
        case IndexedSeq() => clusters
      }

    assignToClusters(clusters, points)

  }



  private def randomSample(take: Int, points: Seq[Point]): List[Point] =
  {
    val r = new Random(100)
    r.shuffle(points).take(take).toList
  }


}
