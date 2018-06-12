package cluster

import cluster.Point.ValueType

import scala.util.Random

class AlgorithmElementsBuilder[T]
{

  type PointValueType = Point.ValueType[T]

  def createPoint(values: ValueType[T], toSingleValue: (ValueType[T]) => T): Point[T] =
    new Point[T](values, toSingleValue)

  def createCluster(id: Int, name: String, points: Vector[Point[T]]): Cluster[T] =
    new Cluster[T](id, name, points)



  def run(numberOfClusters: Int, points: Vector[Point[T]]) =
  {
    println(randomSample(numberOfClusters, points))
  }

  private def randomSample(take: Int, points: Seq[Point[T]]): List[Point[T]] = {
    val r = new Random(100)
    r.shuffle(points).take(take).toList
  }

}

class Algorithm {

}
