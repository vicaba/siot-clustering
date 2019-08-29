package test

import types.clusterer.immutable.Point
import reader.SyntheticProfilesReaderForScheduler._
import test.load.AccumulatedLoad
import types.clusterer.mutable.Cluster

object EuclideanClustererToSchedulerDataTypeTransformation {

  def apply(spanSlotAccumulatedLoadId: Int, points: Set[Point]): AccumulatedLoad = {

    val builder = ApplianceLoadBuilder

    val maxDataPerPoint = points.maxBy(_.dataLabels.length).size

    val allLoads = points.zipWithIndex.flatMap {
      case (p, pIdx) =>
        p.data(breeze.linalg.*, ::).iterator.zip(p.dataLabels.toIterator).zipWithIndex.map {
          case ((dv, label), loadIdx) =>
            builder(pIdx * maxDataPerPoint + loadIdx, dv.toScalaVector(), label)
        }
    }
    AccumulatedLoad(spanSlotAccumulatedLoadId, 0, allLoads)
  }

}
