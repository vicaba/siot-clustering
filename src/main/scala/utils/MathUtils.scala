package utils
import scala.annotation.tailrec

object MathUtils {

  def factorize(x: Int): List[Int] = {
    @tailrec
    def _factorize(x: Int, a: Int = 2, list: List[Int] = Nil): List[Int] = a * a > x match {
      case false if x % a == 0 => _factorize(x / a, a, a :: list)
      case false => _factorize(x, a + 1, list)
      case true  => x :: list
    }
    _factorize(x)
  }

/*
  def clusteringOrder(points: Int, k: Int): List[Int] = {
    val pointsFactors = factorize(points)
    val kFactors = factorize(k)
    val groupedPointsFactors = pointsFactors.groupBy(_)
    val groupedKFactors = kFactors.groupBy(_)
  }
*/

}
