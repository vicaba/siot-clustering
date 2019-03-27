package algorithm.scheduler

import algorithm.Settings
import algorithm.scheduler.Rescheduler.MatrixResult
import metrics.Metric
import types.immutable.Point
import types.mutable.Cluster

case class ReschedulerSettings(override val numberOfClusters: Int, override val metric: Metric, improvement: Double, memory: Int = 2) extends Settings

class PointChange(val cluster: Cluster, val point: Point, val change: MatrixResult[Double])

class PointChanged(val point: Point, val change: MatrixResult[Double])