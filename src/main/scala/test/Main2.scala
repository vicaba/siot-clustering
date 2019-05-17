package test

import test.Element._

import scala.annotation.tailrec

object Main2 {

  type LoadElement = Element[Double]

  class Loads(val fixedLoads: Vector[LoadElement], val flexibleLoads: Vector[LoadElement])

  object Loads {

    def apply(fixedLoads: Vector[Element[Double]], flexibleLoads: Vector[Element[Double]]): Loads =
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

    @tailrec
    def rec(flexibleLoads: Vector[LoadElement], fixedLoads: Vector[LoadElement]): Loads = flexibleLoads match {
      case flexibleLoad +: remainingFlexibleLoads =>
        rec(
          remainingFlexibleLoads,
          fixedLoads.tail :+ fixedLoads.head.copy(approximateValue = Some(flexibleLoad.value))
        )
      case IndexedSeq() => Loads(fixedLoads = fixedLoads, flexibleLoads = flexibleLoads)
    }

    rec(flexibleLoads, fixedLoads)

  }

  def maxPeakOf(v: scala.Seq[LoadElement]): LoadElement = v.max

  def higherThanPeakOrderedDesc(peak: LoadElement, loads: scala.Vector[LoadElement]): scala.Vector[LoadElement] =
    loads filter (_.value >= peak.value) sortWith (_.value > _.value)

}