package scheduler_model.user_allocator.user_representation.conditions

import breeze.linalg.max
import scheduler_model.load.AccumulatedLoad

import scala.util.Try

object MaxPeakGraterThanMaxFixedLoadsPeakCondition extends Condition {
  override def apply(prevResult: Vector[Double], user: AccumulatedLoad, usersFixedLoad: AccumulatedLoad): Boolean =
    Try(prevResult.max).getOrElse(0.0) >= max(usersFixedLoad.amplitudePerSlot)
}
