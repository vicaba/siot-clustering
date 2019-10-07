package scheduler_model

import breeze.linalg._
import scheduler_model.load.{AccumulatedLoad, FlexibleLoadSubTask, FlexibleLoadSuperTask}
import scheduler_model.reader.SyntheticProfilesReaderForScheduler2.ApplianceLoadBuilder
import types.clusterer.DataTypeMetadata
import types.clusterer.immutable.Point
import types.clusterer.mutable.Cluster

object ClusterAndAccumulatedLoadTransformer {

  def apply(clusters: Seq[Cluster]): Seq[AccumulatedLoad] = {

    ???

/*    val builder = ApplianceLoadBuilder

    var applianceCounter: Int = -1

    clusters.map { cluster =>

      val pointsInCluster = Cluster.flatten(cluster)

      pointsInCluster.map { pointInCluster =>

        val dataIterator  = pointInCluster.data(breeze.linalg.*, ::).iterator.toList
        val labelIterator = pointInCluster.dataLabels.toIterator.toList

        def buildAppliance(dv: DenseVector[Double], label: String = "") = {
          applianceCounter = applianceCounter + 1
          builder(applianceCounter, dv.toScalaVector(), label, replaceWithLabel = Some("-p" + pointInCluster.id + "-" + label))
        }

        if (labelIterator.nonEmpty) dataIterator.zip(labelIterator).map {
          case (dv, label) =>
            buildAppliance(dv, label)
        } else
          dataIterator.map { dv =>
            buildAppliance(dv)
          }

      }

    }*/


  }

  def apply(accumulatedLoads: Seq[AccumulatedLoad], dataTypeMetadata: DataTypeMetadata): Seq[Cluster] = {

    val pointRegex = """-p(\d+)-""".r

    accumulatedLoads.map { accumulatedLoad =>
      val cluster = Cluster(accumulatedLoad.id, accumulatedLoad.label, Set.empty[Point], 1, None)(dataTypeMetadata)
      accumulatedLoad.loads.filter(_.isInstanceOf[FlexibleLoadSuperTask]).foreach(_.asInstanceOf[FlexibleLoadSuperTask].computeAmplitudePerSlotWithRestValueOnly = false)
      val points = accumulatedLoad.loads.filter(!_.isInstanceOf[FlexibleLoadSubTask]).toList.groupBy(l => pointRegex.findFirstMatchIn(l.label).get.group(1)).map {
        case ((key, loads)) =>
          val vectorList = loads.map(l => DenseVector(l.amplitudePerSlot.toArray))
          val labelList  = loads.map(_.label)
          val data       = DenseMatrix(vectorList: _*)
          Point(key.toInt, data, labelList, Some(cluster))(dataTypeMetadata)
      }
      cluster ++= points
    }
  }

}
