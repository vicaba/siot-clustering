package algorithm.util

case class RelativeImprovement[T] private (baseValue: Double,
                                           relativeImprovement: Double,
                                           goodInRange: Int,
                                           private val history: List[(Double, T)],
                                           private val lowestGlobalHistory: List[(Double, T)]) {

  def feed(value: Double, e: T): RelativeImprovement[T] = {

    println("historySize: " + history.size)

    if (hasReachedMaxHistory) {
      val historyMin = history.minBy(_._1)
      val _lowestGlobalHistory = if (truncate(historyMin._1) <= truncate(lowestGlobalHistory.minBy(_._1)._1)) {
        historyMin :: lowestGlobalHistory
      } else lowestGlobalHistory
      RelativeImprovement[T](average, relativeImprovement, goodInRange, Nil, _lowestGlobalHistory)
    } else
      this.copy(baseValue,
                relativeImprovement,
                goodInRange,
                (value, e) :: history,
                lowestGlobalHistory)
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
      val truncatedAverage = truncate(average)
      history.count { e =>
        truncate(e._1) == truncatedAverage
        // TODO: Why defaulting to 0.6?
      } >= Math.floor(0.6 * goodInRange) || allElementsOfHistoryAreEqualToAverage
    }

  def allElementsOfHistoryAreEqualToAverage: Boolean =  {
    val equal = history.forall(_._1 == average)
    println("all elements of history are equal to average: " + equal)
    equal
  }

  def getBest: (Double, T) = lowestGlobalHistory.minBy(_._1)

  def hasReachedMaxHistory: Boolean = history.size == goodInRange

  def truncate(n: Double, pos: Int): Double = BigDecimal(n).setScale(pos, BigDecimal.RoundingMode.FLOOR).toDouble

  // TODO: Why defaulting to 3?
  def truncate(n: Double): Double = truncate(n, 3)

}

object RelativeImprovement {
  def apply[T](baseValue: (Double, T), relativeImprovement: Double, goodInRange: Int): RelativeImprovement[T] =
    new RelativeImprovement(baseValue._1, relativeImprovement, goodInRange, Nil, baseValue :: Nil)
}
