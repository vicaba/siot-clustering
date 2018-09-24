package metrics

import breeze.stats._
import metrics.Metric.Progression
import breeze.linalg._
import util.Slicer

trait DenseVectorReprOps[T] {

  def apply(t: T): DenseVector[Double]

  def zero(t: T): DenseVector[Double]

}

object Metric {

  def par: Metric = Par.default

  case class ProgressionResult(distance: Double, progression: Progression)

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

trait MetricCompanion {

  val default: Metric

  trait AggregateOf {
    def apply[T: DenseVectorReprOps](metric: Metric, list: Iterable[T]): Double
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

  val aggregateOf: MetricCompanion#AggregateOf

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
  def aggregateOf[T: DenseVectorReprOps](list: Iterable[T]): Double = aggregateOf.apply(this, list)

}

object Par extends MetricCompanion {

  val withParAggregate: Par = new Par {
    override val aggregateOf: AggregateOf = ParAggregate
  }

  val withAverageAggregate: Par = new Par {
    override val aggregateOf: AggregateOf = AverageAggregate
  }

  val withGeometricAverageAggregate: Par = new Par {
    override val aggregateOf: AggregateOf = GeometricAverageAggregate
  }

  val withGroupedGeometricAverageAggregate: Par = new Par {
    override val aggregateOf: AggregateOf = GroupedGeometricAverageAggregate
  }

  override val default: Metric = withParAggregate

  object ParAggregate extends AggregateOf {

    override def apply[T: DenseVectorReprOps](metric: Metric, list: Iterable[T]): Double = {
      val toVectorOps = implicitly[DenseVectorReprOps[T]]
      if (list.size == 1) metric(list.head)
      else {
        val metricVector = DenseVector[Double](list.map { t =>
          metric(t)
        }.toList: _*)
        metric(metricVector)
      }
    }

    override def toString: String = "par"

  }

  object AverageAggregate extends AggregateOf {

    override def apply[T: DenseVectorReprOps](metric: Metric, list: Iterable[T]): Double = {
      val toVectorOps = implicitly[DenseVectorReprOps[T]]
      if (list.size == 1) metric(list.head) else {
        list.foldLeft(0.0) { case (accum, v) => accum + metric(v) } / list.size
      }
    }

    override def toString: String = "average"

  }

  object GeometricAverageAggregate extends AggregateOf {

    override def apply[T: DenseVectorReprOps](metric: Metric, list: Iterable[T]): Double =
      (list.zipWithIndex
        .map {
          case (e, i) =>
            BigDecimal(metric.apply(e)).pow(i + 1)
        }
        .fold(BigDecimal(0))(_ + _) / list.size).toDouble

    override def toString: String = "grouped geometric average"

  }

  object GroupedGeometricAverageAggregate extends AggregateOf {
    override def apply[T: DenseVectorReprOps](metric: Metric, list: Iterable[T]): Double = {
      val l     = list.map(metric(_)).map(BigDecimal(_))
      val lMax  = l.max
      val lMin  = if (l.size != 1) l.min else BigDecimal(0)
      val steps = (l.size / 2) + 1
      val step  = (lMax - lMin) / steps

      /* Group ranges so elements of the metric list can be compared */
      val groupedRanges = Slicer.slice(lMin, lMax, step)
      val lGeometricGrouped = l.groupBy { e =>
        groupedRanges.find { r =>
          r.start <= e && r.end >= e
        }.get
      }
      (lGeometricGrouped.zipWithIndex
        .map {
          case ((r, e), i) =>
            e.fold(BigDecimal(1))(_ + _.pow(i + 1))
        }
        .fold(BigDecimal(0))(_ + _) / l.size).toDouble
    }
  }

}

trait Par extends Metric {

  override val Highest: Double = Double.PositiveInfinity

  override val Lowest: Double = 1.0

  override def apply(e: DenseVector[Double]): Double = if (!e.forall(_ == 0)) (max(e) / mean(e)) else this.Lowest

  override def progression(before: DenseVector[Double], after: DenseVector[Double]): Progression = {

    val betterParThanBefore  = this(before) > this(after)
    val maxIsLowerThanBefore = max(after) < max(before)
    val maxIsEqualAsBefore   = max(after) == max(before)
    val lessMaxsThanBefore = {
      val maxBefore = max(before)
      val maxAfter  = max(after)
      after.toScalaVector().count(_ == maxAfter) < before.toScalaVector().count(_ == maxBefore)
    }

    if (lessMaxsThanBefore && maxIsEqualAsBefore | maxIsLowerThanBefore)
      Progression.Positive
    else
      Progression.Negative

  }

  override def toString: String = "par, " + aggregateOf.toString

}
