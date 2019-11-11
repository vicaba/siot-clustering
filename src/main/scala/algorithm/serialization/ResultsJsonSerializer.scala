package algorithm.serialization

import algorithm.EuclideanAlgorithm.{ClustererAndReschedulerOutput, ClustererOutput, ReschedulerOutput}
import breeze.linalg.max
import crossfold.CrossFoldValidation.{CrossFoldTypeSettings, MonteCarlo}
import metrics.{Metric, Par}
import play.api.libs.json._

object ResultsJsonSerializer {

  object Keys {

    sealed trait StepKey

    object ClustererKey extends StepKey

    object ReschedulerKey extends StepKey

    val KKey = "k"

    def stepToString(step: StepKey): String = step match {
      case ClustererKey => "s1"
      case ReschedulerKey => "s2"
    }

    def peakKey(step: StepKey): String = stepToString(step) + ". peak"

    def aggregateMetricKey(step: StepKey): String = stepToString(step) + ". agg m"

    def maxMetricKey(step: StepKey): String = stepToString(step) + ". max m"

    def totalMetricKey(step: StepKey) = stepToString(step) + ". total m"

    val ClustersKey = "clusters"

  }

  import Keys._

  object Summary {

    def clustererOutputAsJson(step: ClustererOutput): JsObject =
      Json.obj(
        KKey                  -> step.settings.numberOfClusters,
        peakKey(ClustererKey) -> max(step.clusters.maxBy(c => max(c.syntheticValue)).syntheticValue),
        aggregateMetricKey(ClustererKey) -> step.settings.metric
          .aggregateOf(step.clusters), //steps._1.aggregatedMetric,
        maxMetricKey(ClustererKey) -> step.clusters.map(step.settings.metric(_)).max,
        totalMetricKey(ClustererKey)             -> step.settings.metric(step.clusters),
        ClustersKey                -> step.clusters.map(_.userWiseSize)
      )

    def clustererOutputListAsJson(stepsList: List[ClustererOutput]): List[JsObject] = {
      stepsList.zipWithIndex.map {
        case (clustererOutput, _) =>
          clustererOutputAsJson(clustererOutput)
      }
    }

    def reschedulerOutputAsJson(step: ReschedulerOutput): JsObject =
      Json.obj(
        KKey                    -> step.settings.numberOfClusters,
        peakKey(ReschedulerKey) -> max(step.clusters.maxBy(c => max(c.syntheticValue)).syntheticValue),
        aggregateMetricKey(ReschedulerKey) -> step.settings.metric
          .aggregateOf(step.clusters), //steps._1.aggregatedMetric,
        maxMetricKey(ReschedulerKey) -> step.clusters.map(step.settings.metric(_)).max,
        totalMetricKey(ReschedulerKey)               -> step.settings.metric(step.clusters),
        ClustersKey                  -> step.clusters.map(_.userWiseSize)
      )

    def reschedulerOutputListAsJson(stepsList: List[ReschedulerOutput]): List[JsObject] = {
      stepsList.zipWithIndex.map {
        case (reschedulerOutput, idx) =>
          reschedulerOutputAsJson(reschedulerOutput)
      }
    }

    def batchRunAsJson(stepsList: List[ClustererAndReschedulerOutput]): List[JsObject] = {
      stepsList.zipWithIndex.map {
        case (clustererAndReschedulerOutput, _) =>
          clustererOutputAsJson(clustererAndReschedulerOutput.clustererOutput) ++
            reschedulerOutputAsJson(clustererAndReschedulerOutput.reschedulerOutput)
      }
    }

    def clustererOutputCrossfoldRunAsJson(
        clustererOutputCrossfold: List[List[ClustererOutput]]): List[List[JsObject]] = {
      clustererOutputCrossfold.map(clustererOutputListAsJson)
    }

    def reschedulerOutputCrossfoldRunAsJson(
        reschedulerOutputCrossfold: List[List[ReschedulerOutput]]): List[List[JsObject]] = {
      reschedulerOutputCrossfold.map(reschedulerOutputListAsJson)
    }

    val crossfoldTypeSettingsWriter: OWrites[CrossFoldTypeSettings] = new OWrites[CrossFoldTypeSettings] {
      override def writes(o: CrossFoldTypeSettings): JsObject = o match {
        case m: MonteCarlo =>
          Json.obj(
            "splits"     -> m.splits,
            "sampleSize" -> m.subSampleSize.v
          )
      }
    }

    //TODO: Test
    def crossfoldBatchRunAsJson(
        stepsList: List[(CrossFoldTypeSettings, List[List[ClustererAndReschedulerOutput]])]): List[JsObject] = {
      stepsList.map {
        case (crossFoldSettings, steps) =>
          Json.obj(
            "crossfold" -> crossfoldTypeSettingsWriter.writes(crossFoldSettings),
            "step"      -> steps.map(batchRunAsJson)
          )

      }
    }

    def clustererOutputCrossfoldBatchRunAsJson(
        stepsList: List[(CrossFoldTypeSettings, List[List[ClustererOutput]])]): List[JsObject] = {
      stepsList.map {
        case (crossFoldSettings, steps) =>
          Json.obj(
            "crossfold" -> crossfoldTypeSettingsWriter.writes(crossFoldSettings),
            "step"      -> clustererOutputCrossfoldRunAsJson(steps)
          )

      }
    }

  }

  def batchRunAsJson(stepsList: List[ClustererAndReschedulerOutput])(
      implicit stepsWrites: OWrites[ClustererAndReschedulerOutput]): List[JsObject] = {
    stepsList.zipWithIndex.map {
      case (steps, idx) =>
        Json.obj(
          "run"   -> idx,
          "steps" -> Json.toJson(steps)
        )
    }
  }

  def clustererOutputBatchRunAsJson(stepsList: List[ClustererOutput])(
      implicit stepsWrites: OWrites[ClustererOutput]): List[JsObject] = {
    stepsList.zipWithIndex.map {
      case (steps, idx) =>
        Json.obj(
          "run"   -> idx,
          "steps" -> Json.toJson(steps)
        )
    }
  }

}
