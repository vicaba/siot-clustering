package test

import test.Load._
import test.Loads

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom

object Main {

  def main(args: Array[String]): Unit = {

    val fixedLoads = toFixedLoads(Vector[Double](0, 1, 3, 2, 1, 0))

    val orderedFixedLoads = fixedLoads.sortWith(_.amplitude < _.amplitude)

    val flexibleLoads = toFlexibleLoads(Vector[Double](3, 3, 2, 2, 1))

    val orderedFlexibleLoads = higherThanPeakOrderedDesc(maxPeakOf(fixedLoads), flexibleLoads)

    mergeAll(new Loads(orderedFixedLoads, orderedFlexibleLoads))

  }

  def mergeAll(loads: Loads): Vector[OneSlotAccumulatedLoad] = {

    val accumulatedLoads =
      loads.fixedLoads.sortWith(_.amplitude < _.amplitude).map(fl => OneSlotAccumulatedLoad(fl.positionInT, List(fl)))

    val positiveAndOrderedFlexibleLoads =
      higherThanPeakOrderedDesc(maxPeakOf(loads.fixedLoads), loads.flexibleLoads.filter(_.amplitude >= 0))


    val result1 = merge(positiveAndOrderedFlexibleLoads, accumulatedLoads, accumulatedLoads.size, 0)

    val remainingFlexibleLoads =
      Load
        .flatten(result1)
        .filter(_.isInstanceOf[OneSlotFlexibleLoad]).asInstanceOf[Vector[OneSlotFlexibleLoad]]
        .diff(loads.flexibleLoads)

    merge(remainingFlexibleLoads.sortWith(_.amplitude > _.amplitude), result1.sortWith(_.amplitude < _.amplitude), result1.size, 0)

  }

  @tailrec
  def merge(flexibleLoads: Vector[OneSlotFlexibleLoad],
            accumulatedLoads: Vector[OneSlotAccumulatedLoad],
            accumulatedLoadsSize: Int,
            iterations: Int): Vector[OneSlotAccumulatedLoad] = flexibleLoads match {
    case flexibleLoad +: remainingFlexibleLoads =>
      val assignment =
        accumulatedLoads.tail :+ accumulatedLoads.head.copy(loads = flexibleLoad :: accumulatedLoads.head.loads)

      merge(
        remainingFlexibleLoads,
        if (iterations == accumulatedLoadsSize - 1) assignment.sortWith(_.amplitude < _.amplitude)
        else assignment,
        accumulatedLoadsSize,
        iterations + 1
      )
    case IndexedSeq() => accumulatedLoads
  }

  def maxPeakOf(s: Traversable[Load]): Load = s.max

  def higherThanPeakOrderedDesc[X <: Load, S[A] <: Seq[A]](peak: Load, loads: S[X])(
      implicit cbf: CanBuildFrom[Nothing, X, S[X]]): S[X] =
    loads.filter(_.amplitude >= peak.amplitude).sortWith(_.amplitude > _.amplitude).to[S]

}
