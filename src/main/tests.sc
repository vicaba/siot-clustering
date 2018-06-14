import breeze.linalg._
import cluster.Types.{Cluster, Point}
import cluster._

def distanceTo(point: DenseMatrix[Int], cluster: DenseMatrix[Int]): Int = {
  val sum = point + cluster
  println(sum)
  max(sum) - min(sum)
}


val p = DenseMatrix(1, 2)
val c = DenseMatrix(5, 3)

distanceTo(p, c)

/*
val v = DenseVector(0.0, 0.0, 1.0)

val m = DenseMatrix.fill(4, 3)(2.3)

val m2 = m.copy

m(2, ::) := v.t

m + m2

sum(m + m2, Axis._0)
*/



