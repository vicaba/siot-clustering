package metrics

import breeze.stats._
import metrics.Metric.Progression
import breeze.linalg._

object Metric {

  def par: Par.type = Par

  case class MetricResult(distance: Double, progression: Progression)

  sealed trait Progression {

    def isPositive: Boolean = Progression.isPositive(this)

    def isNegative: Boolean = Progression.isNegative(this)

    def isUndefined(p: Progression): Boolean = Progression.isUndefined(this)

  }

  object Progression {

    object Positive extends Progression

    object Negative extends Progression

    object Undefined extends Progression

    def isPositive(p: Progression): Boolean = if (p == Positive) true else false

    def isNegative(p: Progression): Boolean = if (p == Negative) true else false

    def isUndefined(p: Progression): Boolean = if (p == Undefined) true else false

  }

}

trait Metric {

  /**
    * The highest value the metric can take
    */
  val Highest: Double

  /**
    * The lowest value the metric can take
    */
  val Lowest: Double

  /**
    * Apply this metric to the vector
    * @param e the vector
    * @return the metric
    */
  def apply(e: DenseVector[Double]): Double

  /**
    * Apply this metric to element e
    * @param e the element
    * @tparam T the type of element, context bounded by a representation of the element as a vector
    * @return the metric
    */
  def apply[T: DenseVectorReprOps](e: T): Double = apply(implicitly[DenseVectorReprOps[T]].apply(e))

  /**
    * Is "after" vector better than the "before" vector, will it improve the metric?
    * @param before
    * @param after
    * @return
    */
  def progression(before: DenseVector[Double], after: DenseVector[Double]): Progression

  /**
    * Aggregate metric of a list of elements that can be represented as a vector
    * @param list
    * @tparam T
    * @return
    */
  def aggregateOf[T: DenseVectorReprOps](list: List[T]): Double
}

trait DenseVectorReprOps[T] {
  def apply(t: T): DenseVector[Double]

  def zero(t: T): DenseVector[Double]
}

object Par extends Metric {

  override val Highest: Double = Double.PositiveInfinity

  override val Lowest: Double = 0.0

  override def apply(e: DenseVector[Double]): Double = if (!e.forall(_ == 0)) (max(e) / mean(e)) else 0

  override def progression(before: DenseVector[Double], after: DenseVector[Double]): Progression = {

    val betterParThanBefore = Par(before) > Par(after)
    val maxIsLowerThanBefore = max(after) < max(before)
    val maxIsEqualAsBefore = max(after) == max(before)
    val lessMaxsThanBefore = {
      val maxBefore = max(before)
      val maxAfter = max(after)
      after.toScalaVector().count(_ == maxAfter) < before.toScalaVector().count(_ == maxBefore)
    }

    if (lessMaxsThanBefore && maxIsEqualAsBefore | maxIsLowerThanBefore)
      Progression.Positive
    else
      Progression.Negative

  }

  override def toString: String = "par"

  /**
    * PAR
    */
  /*  override def aggregateOf[T: DenseVectorReprOps](list: List[T]): Double = {
      val toVectorOps = implicitly[DenseVectorReprOps[T]]
      if (list.size == 1) this(toVectorOps(list.head)) else {
        val metricVector = DenseVector[Double](list.map { t =>
          this(toVectorOps(t))
        }:_*)
        this(metricVector)
      }}*/

  /**
    * Average
    * @param list
    * @tparam T
    * @return
    */
  override def aggregateOf[T: DenseVectorReprOps](list: List[T]): Double = {
    val toVectorOps = implicitly[DenseVectorReprOps[T]]
    list.foldLeft(this (toVectorOps.zero(list.head))) { case (accum, v) => accum + this (v) } / list.size
  }

}
