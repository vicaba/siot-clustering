package scheduler_model.user_allocator.user_representation

import scheduler_model.load.{AccumulatedLoad, FlexibleLoadRepresentation}
import scheduler_model.user_allocator.user_representation.conditions.Condition

trait UserRepresentationAsAmplitude {

  val next: Option[(Condition, UserRepresentationAsAmplitude)]

  final def apply(user: FlexibleLoadRepresentation, usersFixedLoad: AccumulatedLoad): Vector[Double] = {
    val amplitudePerSlot = applyInternal(user)
    next
      .map { _next =>
        if (_next._1(amplitudePerSlot, user, usersFixedLoad)) _next._2(user, usersFixedLoad)
        else amplitudePerSlot
      }
      .getOrElse(amplitudePerSlot)
  }

  protected def applyInternal(user: FlexibleLoadRepresentation): Vector[Double]
}

