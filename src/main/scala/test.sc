object RelativeError {

  def apply(left: Double, right: Double, relativeError: Double): Boolean =

  implicit class LeftOperand(val left: Double) {
    def ?+/-(relativeError: Double): Boolean =
      apply(left, left, relativeError)
  }
  implicit class RightOperand(val right: Double) {
    def +/-(relativeError: Double): RelativeError = new RelativeError(right, relativeError)
  }

  class RelativeError(val right: Double, val relativeError: Double) {
    def ~=:(left: Double): Boolean = apply(left, right, relativeError)
  }

}


import RelativeError._

val margin = 5 +/- 3

5 ~=: margin

5 ?+/- 3