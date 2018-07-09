package metrics

import breeze.linalg._
import breeze.stats._
import metrics.Metric.{MetricResult, Progression}
import types.Point
import breeze.linalg._
import breeze.numerics._

object Metric {
  def par: Par.type = Par

  case class MetricResult(distance: Double, progression: Progression)

  sealed trait Progression {

    def positive(): Boolean = Progression.positive(this)

    def negative(): Boolean = Progression.negative(this)

  }

  object Progression {
    object Positive extends Progression
    object Negative extends Progression
    object Undefined extends Progression

    def positive(p: Progression): Boolean = if (p == Positive) true else false

    def negative(p: Progression): Boolean = !positive(p)

  }

}

trait Metric {

  val Highest: Double

  val Lowest: Double

  def apply(e: DenseVector[Double]): Double

  def progression(before: DenseVector[Double], after: DenseVector[Double]): Progression

}

object Par extends Metric {

  override def apply(e: DenseVector[Double]): Double = if (!e.forall(_ == 0)) (max(e) /  mean(e)) - 1 else 0

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

  override val Highest: Double = Double.PositiveInfinity

  override val Lowest: Double = 0.0
}
