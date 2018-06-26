package metrics

import breeze.linalg._
import breeze.stats._
import metrics.Metric.{MetricResult, Progression}

object Metric {
  def par: Par.type = Par
  def maxMin: MaxMin.type = MaxMin

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

  def apply(e: DenseVector[Double]): Double

  def progression(before: DenseVector[Double], after: DenseVector[Double]): Progression

}

object MaxMin extends Metric {

  override def apply(e: DenseVector[Double]): Double = max(e) - min(e)

  override def progression(before: DenseVector[Double], after: DenseVector[Double]): Progression = {

    val betterParThanBefore = Par(before) > Par(after)
    val maxIsLowerThanBefore = max(after) < max(before)
    val lessMaxsThanBefore = {
      val maxBefore = max(before)
      val maxAfter = max(after)
      after.toScalaVector().count(_ == maxAfter) < after.toScalaVector().count(_ == maxBefore)
    }

    if (betterParThanBefore | lessMaxsThanBefore)
      Progression.Positive
    else
      Progression.Negative
  }

}

object Par extends Metric {

  override def apply(e: DenseVector[Double]): Double = max(e) /  mean(e)

  override def progression(before: DenseVector[Double], after: DenseVector[Double]): Progression = {

    val betterParThanBefore = Par(before) > Par(after)
    val maxIsLowerThanBefore = max(after) < max(before)
    val lessMaxsThanBefore = {
      val maxBefore = max(before)
      val maxAfter = max(after)
      after.toScalaVector().count(_ == maxAfter) < before.toScalaVector().count(_ == maxBefore)
    }

    if (betterParThanBefore | (lessMaxsThanBefore && maxIsLowerThanBefore))
      Progression.Positive
    else
      Progression.Negative

  }

}