package algorithm.serialization

import algorithm.algorithms.GenAlgorithm
import breeze.linalg.max
import play.api.libs.json._
import types.Point

object ResultsJsonSerializer {

  def summaryBatchRunAsJson[Algo <: GenAlgorithm](stepsList: List[Algo#Steps]): List[JsObject] = {
    stepsList.zipWithIndex.map {
      case (steps, idx) =>
        Json.obj(
          "k"         -> steps._1.settings.numberOfClusters,
          "s1. peak"  -> max(steps._1.clusters.maxBy(c => max(c.syntheticValue)).syntheticValue),
          "s1. agg m" -> steps._2.settings.metric.aggregateOf(steps._1.clusters), //steps._1.aggregatedMetric,
          "s1. max m" -> steps._1.clusters.map(steps._2.settings.metric(_)).max,
          "s2. peak"  -> max(steps._2.clusters.maxBy(c => max(c.syntheticValue)).syntheticValue),
          "s2. agg m" -> steps._2.settings.metric.aggregateOf(steps._2.clusters),
          "s2. max m" -> steps._2.clusters.map(steps._2.settings.metric(_)).max,
          "total m" -> steps._2.settings.metric(steps._2.clusters),
          "clusters" -> steps._1.clusters.map(_.size)
        )
    }
  }

  def summaryClustererBatchRunAsJson[Algo <: GenAlgorithm](
      stepsList: List[Algo#StepT[Algo#ClustererSettings]]): List[JsObject] = {
    stepsList.zipWithIndex.map {
      case (step, idx) =>
        Json.obj(
          "k"         -> step.settings.numberOfClusters,
          "s1. peak"  -> max(step.clusters.maxBy(c => max(c.syntheticValue)).syntheticValue),
          "s1. agg m" -> step.settings.metric.aggregateOf(step.clusters), //steps._1.aggregatedMetric,
          "s1. max m" -> step.clusters.map(step.settings.metric(_)).max,
          "total m" ->step.settings.metric(step.clusters),
          "clusters" -> step.clusters.map(_.size)
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

  def clustererBatchRunAsJson[Algo <: GenAlgorithm](stepsList: List[Algo#StepT[Algo#ClustererSettings]])(
      implicit stepsWrites: OWrites[Algo#StepT[Algo#ClustererSettings]]): List[JsObject] = {
    stepsList.zipWithIndex.map {
      case (steps, idx) =>
        Json.obj(
          "run"   -> idx,
          "steps" -> Json.toJson(steps)
        )
    }
  }

}
