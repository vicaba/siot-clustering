package scheduler_model.scheduler

import scheduler_model.load.{AccumulatedLoad, FlexibleLoad}

class Movement(
  val acc: AccumulatedLoad,
  val fl: FlexibleLoad,
  val preferredSlots: List[Int],
  val bias: Double = 0.2
)
