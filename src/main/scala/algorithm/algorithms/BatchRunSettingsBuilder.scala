package algorithm.algorithms
import metrics.Metric
import types.immutable.Point
trait BatchRunSettingsBuilder[Algorithm <: GenAlgorithm] {

  type ThisType = BatchRunSettingsBuilder[Algorithm]

  val points: Vector[Point]

  val numbersOfClusters: List[Int]

  val metrics: List[Metric]

  val improveIterations: (Vector[Point], Int) => Int

  def build: List[(Algorithm#ClustererSettingsT, Algorithm#ReschedulerSettingsT)]

  def copy(points: Vector[Point] = points,
           numbersOfClusters: List[Int] = numbersOfClusters,
           metrics: List[Metric] = metrics,
           timesToIterate: (Vector[Point], Int) => Int = improveIterations): ThisType
}
