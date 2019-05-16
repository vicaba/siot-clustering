package test

import test.Element._

object Main2 {

  def main(args: Array[String]): Unit = {

    val fixedLoads = toListOfElements(Vector[Double](0, 1, 3, 2, 1, 0))

    val variableLoads = toListOfElements(Vector[Double](3, 3, 2, 2, 1))

    val orderedVariableLoads = higherThanPeakOrderedDesc(maxPeakOf(fixedLoads), variableLoads)

    fixedLoads.sortWith (_.value < _.value)

  }

  def maxPeakOf(v: scala.Seq[Element[Double]]): Element[Double] = v.max

  def higherThanPeakOrderedDesc(peak: Element[Double], loads: scala.Seq[Element[Double]]): scala.Seq[Element[Double]] =
    loads filter (_.value >= peak.value) sortWith (_.value > _.value)

}
