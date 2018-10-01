package utils
import scala.annotation.tailrec

object Slicer {

  case class RangeT[T](start: T, end: T)

  def slice[T: Numeric](start: T, end: T, step: T): List[RangeT[T]] = {

    val numeric = implicitly[Numeric[T]]

    @tailrec
    def _slice(start: T, end: T, acc: List[RangeT[T]]): List[RangeT[T]] = {
      val shift = numeric.plus(start, step)
      numeric.compare(shift, end) match {
        case x if x > 0 =>
          if (acc.headOption.nonEmpty && numeric.compare(acc.head.end, end) < 0)
            RangeT(acc.head.end, end) :: acc
          else
            acc
        case x if x == 0 => RangeT[T](start, shift) :: acc
        case x if x < 0  => _slice(shift, end, RangeT(start, shift) :: acc)
      }
    }

    _slice(start, end, List[RangeT[T]]()).reverse

  }

  def main(args: Array[String]): Unit = {

    println(slice(BigDecimal(1), BigDecimal(4), BigDecimal(0.5)))

  }

}
