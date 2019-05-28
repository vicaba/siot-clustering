import scala.collection.mutable.ListBuffer

case class RelativeImprovement private (baseValue: Double,
                                        relativeImprovement: Double,
                                        goodInRange: Int,
                                        history: List[Double]) {

  def feed(value: Double): RelativeImprovement = {
    this.copy(baseValue, relativeImprovement, goodInRange, value :: history)
  }

  def hasDecreased: Boolean =
    baseValue > (history.foldLeft(0.0) { case (accum, item) => accum + item } / history.size)

  def hasReachedMaxHistory: Boolean = history.size == goodInRange

}

object RelativeImprovement {
  def apply(baseValue: Double, relativeImprovement: Double, goodInRange: Int): RelativeImprovement =
    new RelativeImprovement(baseValue, relativeImprovement, goodInRange, Nil)
}

BigDecimal(1.23456789).setScale(2, BigDecimal.RoundingMode.FLOOR).toDouble