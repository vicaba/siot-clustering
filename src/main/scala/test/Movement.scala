package test

class Movement(val acc: SpanSlotAccumulatedLoad,
               val fl: SpanSlotFlexibleLoad,
               preferredSlots: List[Int],
               bias: Double = 0.2)
