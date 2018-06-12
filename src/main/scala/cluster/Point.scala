package cluster

import cluster.Point._

object Point {

  type ValueType[T] = Vector[Vector[T]]

}

class Point[T](values: ValueType[T], toSingleValue: (ValueType[T]) => T)
