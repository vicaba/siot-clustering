import scala.collection.mutable

val s = new mutable.HashSet[Int]
s += 1
s += (2, 3)

s --= s

s