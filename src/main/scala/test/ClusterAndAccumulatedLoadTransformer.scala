package test

import breeze.linalg.{DenseMatrix, DenseVector}
import types.clusterer.immutable.Point
import reader.SyntheticProfilesReaderForScheduler._
import test.load.AccumulatedLoad
import types.clusterer.DataTypeMetadata
import types.clusterer.mutable.Cluster

object ClusterAndAccumulatedLoadTransformer {

  def apply(clusters: Seq[Cluster]): Seq[AccumulatedLoad] = {

    val builder = ApplianceLoadBuilder

    clusters.map(Cluster.flatten).zip(clusters).map {
      case (pointsInCluster, cluster) =>
        val loads = pointsInCluster.flatMap { pointInCluster =>
          pointInCluster.data(breeze.linalg.*, ::).iterator.zip(pointInCluster.dataLabels.toIterator).map {
            case (dv, label) =>
              builder(pointInCluster.id, dv.toScalaVector(), "-p" + pointInCluster.id + "-" + label)
          }
        }
        AccumulatedLoad(cluster.id, 0, loads, label = cluster.name)
    }
  }

  def apply(accumulatedLoads: Seq[AccumulatedLoad], dataTypeMetadata: DataTypeMetadata): Seq[Cluster] = {

    val pointRegex = """-p(\d+)-""".r

    accumulatedLoads.map { accumulatedLoad =>
      val cluster = Cluster(accumulatedLoad.id, accumulatedLoad.label, Set.empty[Point], 1, None)(dataTypeMetadata)
      val points = accumulatedLoad.loads.groupBy(l => pointRegex.findFirstMatchIn(l.label).get.group(1)).map {
        case ((key, loads)) =>
          val vectorList = loads.map(l => DenseVector(l.amplitudePerSlot: _*)).toList
          val labelList  = loads.map(_.label).toList
          val data       = DenseMatrix(vectorList: _*)
          Point(key.toInt, data, labelList, Some(cluster))(dataTypeMetadata)
      }
      cluster ++= points
    }
  }
}
