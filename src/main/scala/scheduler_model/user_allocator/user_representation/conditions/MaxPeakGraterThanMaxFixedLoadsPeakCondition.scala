package scheduler_model.user_allocator.user_representation.conditions

import breeze.linalg.{max, min}
import scheduler_model.load.{AccumulatedLoad, FlexibleLoadRepresentation}

import scala.util.Try

object MaxPeakGraterThanMaxFixedLoadsPeakCondition extends Condition {
  override def apply(prevResult: Vector[Double], user: FlexibleLoadRepresentation, usersFixedLoad: AccumulatedLoad): Boolean =
    Try(min(usersFixedLoad.amplitudePerSlot) + prevResult.max).getOrElse(0.0) > max(usersFixedLoad.amplitudePerSlot)
}
