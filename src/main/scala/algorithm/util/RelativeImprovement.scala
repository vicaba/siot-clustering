package algorithm.util

case class RelativeImprovement private (baseValue: Double,
                                        relativeImprovement: Double,
                                        goodInRange: Int,
                                        private val history: List[Double]) {

  def feed(value: Double): RelativeImprovement = {

    if (hasReachedMaxHistory) RelativeImprovement(average, relativeImprovement, goodInRange, Nil)
    else this.copy(baseValue, relativeImprovement, goodInRange, value :: history)
  }

  def average: Double =
    if (history.isEmpty) 0.0
    else
      history.foldLeft(0.0) { case (accum, item) => accum + item } / history.size

  def hasDecreased: Boolean = baseValue > average

  def hasImprovedEnough: Boolean =
    if (history.isEmpty) false
    else hasReachedMaxHistory && (baseValue - average) < relativeImprovement && (history.last - history.head) > 0

  def isNotImprovingEnough: Boolean =
    if (history.isEmpty || !hasReachedMaxHistory) false
    else {
      def truncate(n: Double): Double = BigDecimal(n).setScale(3, BigDecimal.RoundingMode.FLOOR).toDouble
      val truncatedAverage            = truncate(average)
      history.forall(truncate(_) == truncatedAverage)
    }

  def hasReachedMaxHistory: Boolean = history.size == goodInRange

}

object RelativeImprovement {
  def apply(baseValue: Double, relativeImprovement: Double, goodInRange: Int): RelativeImprovement =
    new RelativeImprovement(baseValue, relativeImprovement, goodInRange, Nil)
}
