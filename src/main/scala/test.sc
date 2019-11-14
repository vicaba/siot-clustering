import breeze.linalg
import breeze.linalg._


val a = DenseVector(2, 2, 2)

val b = DenseVector(3, 3, 3)

val c = DenseVector.vertcat(a,b)

c.toString