package scheduler_model.user_allocator.user_representation

import scheduler_model.load.{AccumulatedLoad, FlexibleLoadRepresentation}
import scheduler_model.user_allocator.user_representation.conditions.Condition

import scala.util.Try

class UserRepresentationAsAmplitudeInMaxTimeSpan(
                                                      override val next: Option[(Condition, UserRepresentationAsAmplitude)] = None)
  extends UserRepresentationAsAmplitude {
  override def applyInternal(user: FlexibleLoadRepresentation): Vector[Double] =
    RepresentUserAsAmplitude(user, windowSize = user.maxTimeSpan)
}

