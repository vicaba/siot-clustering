package test

import test.Load._
import test.Loads

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom

object Main2 {

  def main(args: Array[String]): Unit = {

    val fixedLoads = toFixedLoads(Vector[Double](0, 1, 3, 2, 1, 0))

    val orderedFixedLoads = fixedLoads.sortWith (_.amplitude < _.amplitude)

    val flexibleLoads = toFlexibleLoads(Vector[Double](3, 3, 2, 2, 1))

    val orderedFlexibleLoads = higherThanPeakOrderedDesc(maxPeakOf(fixedLoads), flexibleLoads)

    merge(new Loads(orderedFixedLoads, orderedFlexibleLoads))

  }

  def merge(loads: Loads): Vector[AccumulatedLoad] = {
    val flexibleLoads = loads.flexibleLoads
    val accumulatedLoads = loads.fixedLoads.map(fl => AccumulatedLoad(fl.positionInT, List(fl)))
    val accumulatedLoadsSize = accumulatedLoads.size


    @tailrec
    def rec(flexibleLoads: Vector[FlexibleLoad], accumulatedLoads: Vector[AccumulatedLoad], iterations: Int): Vector[AccumulatedLoad] = flexibleLoads match {
      case flexibleLoad +: remainingFlexibleLoads =>
        val assignment =
          accumulatedLoads.tail :+ accumulatedLoads.head.copy(loads = flexibleLoad :: accumulatedLoads.head.loads)

        rec(
          remainingFlexibleLoads,
          if (iterations == accumulatedLoadsSize - 1) assignment.sortWith(_.amplitude < _.amplitude)
          else assignment,
          iterations + 1
        )
      case IndexedSeq() => accumulatedLoads
    }

      rec(flexibleLoads, accumulatedLoads, 0)

  }

  def maxPeakOf[X <: Load, S[A] <: Seq[A]](s: Seq[Load]): Load = s.max

  def higherThanPeakOrderedDesc[X <: Load, S[A] <: Seq[A]](peak: Load, loads: S[X])(implicit cbf: CanBuildFrom[Nothing, X, S[X]]): S[X] =
    loads.filter(_.amplitude >= peak.amplitude).sortWith(_.amplitude > _.amplitude).to[S]

}