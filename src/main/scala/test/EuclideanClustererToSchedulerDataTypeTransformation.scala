package test

import types.immutable.Point
import reader.SyntheticProfilesReaderForScheduler._

object EuclideanClustererToSchedulerDataTypeTransformation {

  def apply(spanSlotAccumulatedLoadId: Int, points: Set[Point]): SpanSlotAccumulatedLoad = {

    val builder = ApplianceLoadBuilder

    val maxDataPerPoint = points.maxBy(_.dataLabels.length).size

    val allLoads = points.zipWithIndex.flatMap {
      case (p, pIdx) =>
        p.data(breeze.linalg.*, ::).iterator.zip(p.dataLabels.toIterator).zipWithIndex.map {
          case ((dv, label), loadIdx) =>
            builder(pIdx * maxDataPerPoint + loadIdx, dv.toScalaVector(), label)
        }
    }
    SpanSlotAccumulatedLoad(spanSlotAccumulatedLoadId, 0, allLoads)
  }

}
