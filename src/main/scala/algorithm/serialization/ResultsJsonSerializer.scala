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
          "s1. agg m" -> "a", //steps._1.aggregatedMetric,
          "s1. max m" -> steps._1.clusters.map(steps._2.settings.metric(_)).max,
          "s2. peak"  -> max(steps._2.clusters.maxBy(c => max(c.syntheticValue)).syntheticValue),
          "s2. agg m" -> "a", //steps._2.aggregatedMetric,
          "s2. max m" -> steps._2.clusters.map(steps._2.settings.metric(_)).max,
          "total m" -> Point
            .pointListToVector(steps._2.clusters.flatMap(_.points))
            .map(vec => steps._2.settings.metric(vec)),
          "clusters" -> steps._1.clusters.map(_.points.size)
        )
    }
  }

  def batchRunAsJson[Algo <: GenAlgorithm](stepsList: List[Algo#Steps])(implicit stepsWrites: OWrites[Algo#Steps]): List[JsObject] = {
    stepsList.zipWithIndex.map {
      case (steps, idx) =>
        Json.obj(
          "run"   -> idx,
          "steps" -> Json.toJson(steps)
        )
    }
  }

}
