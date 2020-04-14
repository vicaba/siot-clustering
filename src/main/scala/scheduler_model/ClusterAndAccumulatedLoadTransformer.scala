package scheduler_model

import breeze.linalg._
import _root_.reader.TemplateForSyntheticProfilesReader
import _root_.reader.EgaugeFlexibleLoads
import scheduler_model.load.Load.{GroupId, LoadId}
import scheduler_model.load._
import types.clusterer.DataTypeMetadata
import types.clusterer.immutable.Point
import types.clusterer.mutable.Cluster

object ClusterAndAccumulatedLoadTransformer {

  def createAccumulatedLoad(id: LoadId, group: GroupId, label: String, loads: Iterable[Load], amplitudePerSlotMetadata: DataTypeMetadata): AccumulatedLoad =
    AccumulatedLoad(id, group, label, loads)(amplitudePerSlotMetadata)


  def apply(clusters: Seq[Cluster], dataTypeMetadata: DataTypeMetadata): Seq[AccumulatedLoad] = {

    def createLoad(id: LoadId, groupId: GroupId, values: DenseVector[Double], label: String): SingleLoad = {

      def createFlexibleLoad(): FlexibleLoad =
        FlexibleLoad(id, groupId, label, 0, DenseVector[Double](values.toScalaVector():_*))

      def createFixedLoad(): FixedLoad = FixedLoad(id, groupId, label, DenseVector[Double](values.toScalaVector():_*))

      if (TemplateForSyntheticProfilesReader.FlexibleLoads.contains(label)) createFlexibleLoad()
      else createFixedLoad()

    }


    clusters.map { cluster =>

      val pointsInCluster = Cluster.flatten(cluster)

      val loads = pointsInCluster.flatMap { pointInCluster =>

        var applianceCounter: Int = -1
        val dataIterator  = pointInCluster.data(breeze.linalg.*, ::).iterator.toList
        val labelIterator = pointInCluster.dataLabels.toIterator.toList

        def buildAppliance(dv: DenseVector[Double], label: String = "") = {
          applianceCounter = applianceCounter + 1
          createLoad(applianceCounter, pointInCluster.id, dv, label)
        }
        dataIterator.zip(labelIterator).map {
          case (dv, label) =>
            buildAppliance(dv, label)
        }
      }
      createAccumulatedLoad(cluster.id, cluster.id, cluster.name, loads, dataTypeMetadata)
    }
  }

  def reverse(accumulatedLoads: Seq[AccumulatedLoad], dataTypeMetadata: DataTypeMetadata): Seq[Cluster] = {

    accumulatedLoads.map { accumulatedLoad =>
      val cluster = Cluster(accumulatedLoad.id, accumulatedLoad.label, Set.empty[Point], 1, None)(dataTypeMetadata)
      accumulatedLoad.loads.filter(_.isInstanceOf[FlexibleLoadSuperTask]).foreach(_.asInstanceOf[FlexibleLoadSuperTask].computeAmplitudePerSlotWithRestValueOnly = false)
      val points = accumulatedLoad.loads.filter(!_.isInstanceOf[FlexibleLoadSubTask]).toList.groupBy(l => l.group).map {
        case ((group, loads)) =>
          val vectorList = loads.map(l => l.amplitudePerSlot.toDenseVector)
          val labelList  = loads.map(_.label)
          val data       = DenseMatrix(vectorList: _*)
          Point(group, data, labelList, Some(cluster))(dataTypeMetadata)
      }
      cluster ++= points
    }
  }

}
