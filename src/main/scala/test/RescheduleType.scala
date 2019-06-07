package test

object RescheduleType extends Enumeration {
  type RescheduleType = Value
  val MinimizePeak, MinimizeMeanDistance, BiasedPeak = Value
}
