package test

import breeze.linalg.DenseVector
import metrics.{DenseVectorReprOps, Metric}
import org.scalatest.{FeatureSpec, GivenWhenThen}
import org.scalatest.Matchers._
import test.Element.toListOfElements
import test.Main2.{Loads, higherThanPeakOrderedDesc, maxPeakOf, merge}

class Main2Spec extends FeatureSpec with GivenWhenThen
{

  implicit def seqToDenseVector[S <: Seq[Double]]: DenseVectorReprOps[S] = new DenseVectorReprOps[S] {

    override def apply(t: S): DenseVector[Double] = DenseVector(t:_*)

    override def zero(t: S): DenseVector[Double] = DenseVector((for(_ <- 1 to t.size) yield 0.0):_*)
  }

  feature("Merge fixed and flexible loads will reduce PAR") {
    scenario("the amount of flexible loads is less than the amount of fixed loads. When merged, " +
      "PAR(fixedLoads) > PAR(merge(fixedLoads + flexibleLoads))") {

      Given("6 fixed loads")
      val fixedLoads = toListOfElements(Vector[Double](0, 1, 3, 2, 1, 0))

      Given("5 flexible loads")

      val flexibleLoads = toListOfElements(Vector[Double](3, 3, 2, 2, 1))

      When("Merged")

      val orderedFixedLoads = fixedLoads.sortWith (_.value < _.value)
      val orderedFlexibleLoads = higherThanPeakOrderedDesc(maxPeakOf(fixedLoads), flexibleLoads)

      val mergeResult = merge(Loads(orderedFixedLoads, orderedFlexibleLoads))

      Then("PAR(fixedLoads) > PAR(merge(fixedLoads + flexibleLoads))")

      val metricFixedLoads = Metric.par(fixedLoads)
      val metricMergeFixedLoadsWithFlexibleLoads =
        Metric.par(mergeResult.fixedLoads.map {fl => fl.approximateValue.getOrElse(fl.value)})

      metricFixedLoads should be > metricMergeFixedLoadsWithFlexibleLoads
      info(s"Metric.par(fixedLoads): $metricFixedLoads; Metric.par(mergeResult.fixedLoads): $metricMergeFixedLoadsWithFlexibleLoads")

    }

    scenario("the amount of flexible loads is more than the amount of fixed loads. When merged, " +
      "PAR(fixedLoads) > PAR(merge(fixedLoads + flexibleLoads))") {

      Given("6 fixed loads")
      val fixedLoads = toListOfElements(Vector[Double](0, 1, 3, 2, 1, 0))

      Given("8 flexible loads")

      val flexibleLoads = toListOfElements(Vector[Double](3, 3, 2, 2, 1, 1, 1, 1))

      When("Merged")

      val orderedFixedLoads = fixedLoads.sortWith (_.value < _.value)
      val orderedFlexibleLoads = higherThanPeakOrderedDesc(maxPeakOf(fixedLoads), flexibleLoads)

      val mergeResult = merge(Loads(orderedFixedLoads, orderedFlexibleLoads))

      Then("PAR(fixedLoads) > PAR(merge(fixedLoads + flexibleLoads))")

      val metricFixedLoads = Metric.par(fixedLoads)
      val metricMergeFixedLoadsWithFlexibleLoads =
        Metric.par(mergeResult.fixedLoads.map {fl => fl.approximateValue.getOrElse(fl.value)})

      metricFixedLoads should be > metricMergeFixedLoadsWithFlexibleLoads
      info(s"Metric.par(fixedLoads): $metricFixedLoads; Metric.par(mergeResult.fixedLoads): $metricMergeFixedLoadsWithFlexibleLoads")

    }
  }

}
