package test

import test.Load._

object Main2 {

  def main(args: Array[String]): Unit = {

    //val fixedLoads = SpanSlotAccumulatedLoad(toFixedLoads(Vector[Double](1, 1, 1, 1, 1, 1, 1, 1))

    val flexibleLoads = Vector(
      SpanSlotFlexibleLoad(1, 0, 4, Vector[Double](1, 1, 1, 1)),
      SpanSlotFlexibleLoad(1, 3, 4, Vector[Double](1, 1, 1, 1))
    )

    //val res1 = findBestContiguousSlots(fixedLoads, flexibleLoads(0))


  }

/*  /**
  *
    * @param flexibleLoad
    * @return the index of flexibleLoads where the flexible load start is better suited to reduce PAR
    */
  def findBestContiguousSlots(accumulatedLoad: SpanSlotAccumulatedLoad, flexibleLoad: SpanSlotFlexibleLoad): Int = {
    (for (i <- 0 until (fixedLoads.size - flexibleLoad.span)) yield {
      (i,
       flexibleLoad.amplitudePerSlot
         .zip(fixedLoads.map(_.amplitude).slice(i, flexibleLoad.span + i))
         .map { case (fl, fi) => Math.pow(fl + fi, 2) }
         .sum)
    }).max((x: (Int, Double), y: (Int, Double)) => implicitly[Ordering[Double]].compare(x._2, y._2))._1
  }*/

}
