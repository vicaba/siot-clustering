package scheduler_model.user_allocator.user_representation.conditions

import scheduler_model.load.{AccumulatedLoad, FlexibleLoadRepresentation}

trait Condition extends ((Vector[Double], FlexibleLoadRepresentation, AccumulatedLoad) => Boolean) {
  override def apply(prevResult: Vector[Double], user: FlexibleLoadRepresentation, usersFixedLoad: AccumulatedLoad): Boolean
}