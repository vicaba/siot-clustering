import utils.Slicer
val l = List(1.6, 1.3, 1.25, 1.2).map(BigDecimal(_)).sorted

val lMax = l.max
val lMin = if (l.length != 1) l.min else BigDecimal(0)
val steps = BigDecimal(l.length / 2) + 1
val step = (lMax - lMin) / steps

/* Group ranges so elements of the metric list can be compared */
val groupedRanges = Slicer.slice(lMin, lMax, step)

val lGeometricGrouped = l.groupBy { e =>
  groupedRanges.find { r =>
    r.start <= e && r.end >= e
  }.get
}

lGeometricGrouped.zipWithIndex.map { case ((r, e), i) =>
  e.fold(BigDecimal(1))(_ + _.pow(i + 1))
}.fold(BigDecimal(0))(_ + _) / l.length

l.zipWithIndex.map { case (e, i) =>
  e.pow(i + 1)
}.fold(BigDecimal(0))(_ + _) / l.length









