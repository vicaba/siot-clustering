package algorithm.util

case class RelativeImprovement[T] private (baseValue: Double,
                                           relativeImprovement: Double,
                                           goodInRange: Int,
                                           private val history: List[(Double, T)],
                                           private val lowestGlobalHistory: List[(Double, T)]) {

  def feed(value: Double, e: T): RelativeImprovement[T] = {

    if (hasReachedMaxHistory) {
      val historyMin = history.minBy(_._1)
      val _lowestGlobalHistory = if (truncate(historyMin._1) <= truncate(lowestGlobalHistory.minBy(_._1)._1)) {
        historyMin :: lowestGlobalHistory
      } else lowestGlobalHistory
      RelativeImprovement[T](average, relativeImprovement, goodInRange, Nil, _lowestGlobalHistory)
    }
    else this.copy(baseValue, relativeImprovement, goodInRange, (value, e) :: history, lowestGlobalHistory)
  }

  def average: Double =
    if (history.isEmpty) 0.0
    else
      history.foldLeft(0.0) { case (accum, item) => accum + item._1 } / history.size

  def hasDecreased: Boolean = baseValue > average

  def hasImprovedEnough: Boolean =
    if (history.isEmpty) false
    else hasReachedMaxHistory && (baseValue - average) < relativeImprovement

  def isStuck: Boolean =
    if (history.isEmpty || !hasReachedMaxHistory) false
    else {
      val truncatedAverage            = truncate(average)
      history.count { e => truncate(e._1) == truncatedAverage } >= Math.floor(0.6 * goodInRange)
    }

  def getBest: (Double, T) = lowestGlobalHistory.minBy(_._1)

  def hasReachedMaxHistory: Boolean = history.size == goodInRange

  def truncate(n: Double): Double = BigDecimal(n).setScale(3, BigDecimal.RoundingMode.FLOOR).toDouble

}

object RelativeImprovement {
  def apply[T](baseValue: (Double, T), relativeImprovement: Double, goodInRange: Int): RelativeImprovement[T] =
    new RelativeImprovement(baseValue._1, relativeImprovement, goodInRange, Nil, baseValue :: Nil)
}
