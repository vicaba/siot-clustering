package scheduler_model.user_allocator.user_representation

import scheduler_model.load.AccumulatedLoad
import scheduler_model.user_allocator.user_representation.conditions.Condition

import scala.util.Try

class UserRepresentationAsAmplitudeInMaxTimeSpan(
                                                      override val next: Option[(Condition, UserRepresentationAsAmplitude)] = None)
  extends UserRepresentationAsAmplitude {
  override def applyInternal(user: AccumulatedLoad): Vector[Double] =
    RepresentUserAsAmplitude(user, windowSize = Try(user.flexibleLoads.toList.map(_.span).sum).getOrElse(0))
}

