package test

import breeze.linalg.DenseVector
import metrics.{DenseVectorReprOps, Metric}
import org.scalatest.{FeatureSpec, GivenWhenThen}
import org.scalatest.Matchers._
import test.Load._
import test.Loads
import test._
import test.Main2.{higherThanPeakOrderedDesc, maxPeakOf, merge}

class Main2Spec extends FeatureSpec with GivenWhenThen {

  implicit def seqToDenseVector[S <: Seq[Double]]: DenseVectorReprOps[S] = new DenseVectorReprOps[S] {

    override def apply(t: S): DenseVector[Double] = DenseVector(t: _*)

    override def zero(t: S): DenseVector[Double] = DenseVector((for (_ <- 1 to t.size) yield 0.0): _*)
  }

  feature("Merge fixed and flexible loads will reduce PAR") {

    scenario(
      "The amount of flexible loads is less than the amount of fixed loads. When merged, " +
        "PAR(fixedLoads) > PAR(merge(fixedLoads + flexibleLoads))") {

      Given("6 fixed loads")

      val fixedLoads = toFixedLoads(Vector[Double](0, 1, 3, 2, 1, 0))

      Given("5 flexible loads")

      val flexibleLoads = toFlexibleLoads(Vector[Double](3, 3, 2, 2, 1))

      When("merged")

      val mergeResult = merge(new Loads(fixedLoads, flexibleLoads))

      Then("PAR(fixedLoads) > PAR(merge(fixedLoads + flexibleLoads))")

      val metricFixedLoads = Metric.par(fixedLoads)(Load.toVector)
      val metricMergeFixedLoadsWithFlexibleLoads =
        Metric.par(mergeResult)

      metricFixedLoads should be > metricMergeFixedLoadsWithFlexibleLoads
      info(
        s"Metric.par(fixedLoads): $metricFixedLoads; Metric.par(mergeResult.fixedLoads): $metricMergeFixedLoadsWithFlexibleLoads")

    }

    scenario(
      "The amount of flexible loads is more than the amount of fixed loads. When merged, " +
        "PAR(fixedLoads) > PAR(merge(fixedLoads + flexibleLoads))") {

      Given("6 fixed loads")

      val fixedLoads = toFixedLoads(Vector[Double](0, 1, 3, 2, 1, 0))

      Given("8 flexible loads")

      val flexibleLoads = toFlexibleLoads(Vector[Double](3, 3, 2, 2, 1, 1, 1, 1))

      When("merged")

      val mergeResult = merge(new Loads(fixedLoads, flexibleLoads))

      Then("PAR(fixedLoads) > PAR(merge(fixedLoads + flexibleLoads))")

      val metricFixedLoads = Metric.par(fixedLoads)
      val metricMergeFixedLoadsWithFlexibleLoads =
        Metric.par(mergeResult)

      metricFixedLoads should be > metricMergeFixedLoadsWithFlexibleLoads
      info(
        s"Metric.par(fixedLoads): $metricFixedLoads; Metric.par(mergeResult.fixedLoads): $metricMergeFixedLoadsWithFlexibleLoads")

    }

    scenario(
      "All flexible loads are higher in amplitude than the fixedLoad peak. When merged, " +
        "PAR(fixedLoads) > PAR(merge(fixedLoads + flexibleLoads))") {

      Given("6 fixed loads")

      val fixedLoads = toFixedLoads(Vector[Double](0, 1, 3, 2, 1, 0))

      Given("8 flexible loads")

      val flexibleLoads =
        toFlexibleLoads((for (_ <- 1 to fixedLoads.size + 2) yield maxPeakOf(fixedLoads).amplitude + 2).toVector)

      When("merged")

      val mergeResult = merge(new Loads(fixedLoads, flexibleLoads))

      Then("PAR(fixedLoads) > PAR(merge(fixedLoads + flexibleLoads))")

      val metricFixedLoads = Metric.par(fixedLoads)
      val metricMergeFixedLoadsWithFlexibleLoads =
        Metric.par(mergeResult)

      metricFixedLoads should be > metricMergeFixedLoadsWithFlexibleLoads
      info(
        s"Metric.par(fixedLoads): $metricFixedLoads; Metric.par(mergeResult.fixedLoads): $metricMergeFixedLoadsWithFlexibleLoads")

      Then("all flexible loads must be accumulated")

      (Load.flatten(mergeResult).toSet -- flexibleLoads) shouldBe empty

    }

  }
}
