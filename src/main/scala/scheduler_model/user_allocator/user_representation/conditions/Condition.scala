package scheduler_model.user_allocator.user_representation.conditions

import scheduler_model.load.AccumulatedLoad

trait Condition extends ((Vector[Double], AccumulatedLoad) => Boolean) {
  override def apply(prevResult: Vector[Double], user: AccumulatedLoad): Boolean
}