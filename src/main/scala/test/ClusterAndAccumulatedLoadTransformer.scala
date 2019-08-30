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

    var applianceCounter: Int = -1

    clusters.map(Cluster.flatten).zip(clusters).map {
      case (pointsInCluster, cluster) =>
        val loads = pointsInCluster.flatMap { pointInCluster =>

          val dataIterator  = pointInCluster.data(breeze.linalg.*, ::).iterator.toList
          val labelIterator = pointInCluster.dataLabels.toIterator.toList

          def buildAppliance(dv: DenseVector[Double], label: String = "") = {
            applianceCounter = applianceCounter + 1
            builder(applianceCounter, dv.toScalaVector(), "-p" + pointInCluster.id + "-" + label)
          }

          if (labelIterator.nonEmpty) dataIterator.zip(labelIterator).map {
            case (dv, label) =>
              buildAppliance(dv, label)
          } else
            dataIterator.map { dv =>
              buildAppliance(dv)
            }
        }
        AccumulatedLoad(cluster.id, 0, loads, label = cluster.name)
    }
  }

  def apply(accumulatedLoads: Seq[AccumulatedLoad], dataTypeMetadata: DataTypeMetadata): Seq[Cluster] = {

    val pointRegex = """-p(\d+)-""".r

    accumulatedLoads.map { accumulatedLoad =>
      val cluster = Cluster(accumulatedLoad.id, accumulatedLoad.label, Set.empty[Point], 1, None)(dataTypeMetadata)
      val points = accumulatedLoad.loads.toList.groupBy(l => pointRegex.findFirstMatchIn(l.label).get.group(1)).map {
        case ((key, loads)) =>
          val vectorList = loads.map(l => DenseVector(l.amplitudePerSlot: _*))
          val labelList  = loads.map(_.label)
          val data       = DenseMatrix(vectorList: _*)
          Point(key.toInt, data, labelList, Some(cluster))(dataTypeMetadata)
      }
      cluster ++= points
    }
  }
}
