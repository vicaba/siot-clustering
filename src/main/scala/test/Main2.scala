package test

import test.Element._

import scala.annotation.tailrec

object Main2 {

  type LoadElement = Element

  class Loads(val fixedLoads: Vector[LoadElement], val flexibleLoads: Vector[LoadElement])

  object Loads {

    def apply(fixedLoads: Vector[Element], flexibleLoads: Vector[Element]): Loads =
      new Loads(fixedLoads, flexibleLoads)
  }

  def main(args: Array[String]): Unit = {

    val fixedLoads = toListOfElements(Vector[Double](0, 1, 3, 2, 1, 0))

    val orderedFixedLoads = fixedLoads.sortWith (_.value < _.value)

    val flexibleLoads = toListOfElements(Vector[Double](3, 3, 2, 2, 1))

    val orderedFlexibleLoads = higherThanPeakOrderedDesc(maxPeakOf(fixedLoads), flexibleLoads)

    merge(Loads(orderedFlexibleLoads, orderedFixedLoads))

  }

  def merge(loads: Loads): Loads = {
    val flexibleLoads = loads.flexibleLoads
    val fixedLoads = loads.fixedLoads
    val fixedLoadsSize = fixedLoads.size

    @tailrec
    def rec(flexibleLoads: Vector[LoadElement], fixedLoads: Vector[LoadElement], iterations: Int): Loads = flexibleLoads match {
      case flexibleLoad +: remainingFlexibleLoads =>
        val assignment =
          fixedLoads.tail :+ fixedLoads.head.copy(addedFlexibleLoads = flexibleLoad :: fixedLoads.head.addedFlexibleLoads)

        rec(
          remainingFlexibleLoads,
          if (iterations == fixedLoadsSize - 1) assignment sortWith (_.value < _.value)
          else assignment,
          iterations + 1
        )
      case IndexedSeq() => Loads(fixedLoads = fixedLoads, flexibleLoads = flexibleLoads)
    }

      rec(flexibleLoads, fixedLoads, 0)

  }

  def maxPeakOf(v: scala.Seq[LoadElement]): LoadElement = v.max

  def higherThanPeakOrderedDesc(peak: LoadElement, loads: scala.Vector[LoadElement]): scala.Vector[LoadElement] =
    loads filter (_.value >= peak.value) sortWith (_.value > _.value)

}