package test

import test.load.{AccumulatedLoad, FlexibleLoad}

class Movement(val acc: AccumulatedLoad,
               val fl: FlexibleLoad,
               preferredSlots: List[Int],
               bias: Double = 0.2)
