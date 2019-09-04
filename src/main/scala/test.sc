import breeze.linalg._

val a1a = DenseVector(1.0, 1.0)
val a2a = DenseVector(1.0, 2.0)
val a3a = DenseVector(2.0, 2.0)

val a1b = DenseVector(0.0, 0.0)
val a2b = DenseVector(5.0, 5.0)
val a3b = DenseVector(3.0, 3.0)

val la = List(a1a, a2a, a3a)
val lb = List(a1b, a2b, a3b)

val lac = sum(la) / la.size.toDouble
val lbc = sum(lb) / lb.size.toDouble

lac
lbc

val cluster = List(la, lb)
val expandedCluster = cluster.flatten

val clusterc0 = sum(List(lac, lbc)) / 2.toDouble

val clusterc1 = sum(expandedCluster) / expandedCluster.size.toDouble
