package algorithm.serialization

import algorithm.algorithms.GenAlgorithm
import breeze.linalg.max
import crossfold.CrossFoldValidation.{CrossFoldTypeSettings, MonteCarlo}
import play.api.libs.json._
import types.Point

object ResultsJsonSerializer {

  object Keys {

    sealed trait Step

    object Step1 extends Step

    object Step2 extends Step

    val KKey = "k"

    def stepToString(step: Step): String = step match {
      case Step1 => "s1"
      case Step2 => "s2"
    }

    def peakKey(step: Step): String = stepToString(step) + ". peak"

    def aggregateMetricKey(step: Step): String = stepToString(step) + ". agg m"

    def maxMetricKey(step: Step): String = stepToString(step) + ". max m"

    val TotalMetricKey = "total m"

    val ClustersKey = "clusters"

  }

  import Keys._

  def summaryStep1AsJson[Algo <: GenAlgorithm](step: Algo#StepT[Algo#ClustererSettingsT]): JsObject =
    Json.obj(
      KKey                      -> step.settings.numberOfClusters,
      peakKey(Step1)            -> max(step.clusters.maxBy(c => max(c.syntheticValue)).syntheticValue),
      aggregateMetricKey(Step1) -> step.settings.metric.aggregateOf(step.clusters), //steps._1.aggregatedMetric,
      maxMetricKey(Step1)       -> step.clusters.map(step.settings.metric(_)).max,
      TotalMetricKey            -> step.settings.metric(step.clusters),
      ClustersKey               -> step.clusters.map(_.size)
    )

  def summaryStep1ListAsJson[Algo <: GenAlgorithm](
      stepsList: List[Algo#StepT[Algo#ClustererSettingsT]]): List[JsObject] = {
    stepsList.zipWithIndex.map {
      case (step, idx) =>
        summaryStep1AsJson(step)
    }
  }

  def summaryStep2AsJson[Algo <: GenAlgorithm](step: Algo#StepT[Algo#ReschedulerSettingsT]): JsObject =
    Json.obj(
      KKey                      -> step.settings.numberOfClusters,
      peakKey(Step2)            -> max(step.clusters.maxBy(c => max(c.syntheticValue)).syntheticValue),
      aggregateMetricKey(Step2) -> step.settings.metric.aggregateOf(step.clusters), //steps._1.aggregatedMetric,
      maxMetricKey(Step2)       -> step.clusters.map(step.settings.metric(_)).max,
      TotalMetricKey            -> step.settings.metric(step.clusters),
      ClustersKey               -> step.clusters.map(_.size)
    )

  def summaryStep2ListAsJson[Algo <: GenAlgorithm](
      stepsList: List[Algo#StepT[Algo#ReschedulerSettingsT]]): List[JsObject] = {
    stepsList.zipWithIndex.map {
      case (step, idx) =>
        summaryStep2AsJson(step)
    }
  }

  def summaryBatchRunAsJson[Algo <: GenAlgorithm](stepsList: List[Algo#Steps]): List[JsObject] = {
    stepsList.zipWithIndex.map {
      case (steps, idx) =>
        summaryStep1AsJson(steps._1) ++ summaryStep2AsJson(steps._2)
    }
  }

  def summaryStep1CrossfoldRunAsJson[Algo <: GenAlgorithm](
      stepsList: List[List[Algo#StepT[Algo#ClustererSettingsT]]]): List[List[JsObject]] = {
    stepsList.map(summaryStep1ListAsJson)
  }

  def summaryStep2CrossfoldRunAsJson[Algo <: GenAlgorithm](
      stepsList: List[List[Algo#StepT[Algo#ReschedulerSettingsT]]]): List[List[JsObject]] = {
    stepsList.map(summaryStep2ListAsJson)
  }

  val crossfoldTypeSettingsWriter = new OWrites[CrossFoldTypeSettings] {
    override def writes(o: CrossFoldTypeSettings): JsObject = o match {
      case m: MonteCarlo =>
        Json.obj(
          "splits"     -> m.splits,
          "sampleSize" -> m.subsampleSize.v
        )
    }
  }

  //TODO: Test
  def summaryCrossfoldBatchRunAsJson[Algo <: GenAlgorithm](
      stepsList: List[(CrossFoldTypeSettings, List[List[Algo#Steps]])]): List[JsObject] = {
    stepsList.map {
      case (crossFoldSettings, steps) =>
        Json.obj(
          "crossfold" -> crossfoldTypeSettingsWriter.writes(crossFoldSettings),
          "step"      -> steps.map(summaryBatchRunAsJson)
          //summaryStep1CrossfoldRunAsJson(steps)
        )

    }
  }

  def batchRunAsJson[Algo <: GenAlgorithm](stepsList: List[Algo#Steps])(
      implicit stepsWrites: OWrites[Algo#Steps]): List[JsObject] = {
    stepsList.zipWithIndex.map {
      case (steps, idx) =>
        Json.obj(
          "run"   -> idx,
          "steps" -> Json.toJson(steps)
        )
    }
  }

  def clustererBatchRunAsJson[Algo <: GenAlgorithm](stepsList: List[Algo#StepT[Algo#ClustererSettingsT]])(
      implicit stepsWrites: OWrites[Algo#StepT[Algo#ClustererSettingsT]]): List[JsObject] = {
    stepsList.zipWithIndex.map {
      case (steps, idx) =>
        Json.obj(
          "run"   -> idx,
          "steps" -> Json.toJson(steps)
        )
    }
  }

}
