val l = List(1.6, 1.3, 2, 1.2).map(BigDecimal(_)).sorted

def isOdd(n: BigDecimal) = n % 2 != 0


val lMax = l.max
val lMin = if (l.length != 1) l.min else BigDecimal(0)
val steps = BigDecimal(l.length / 2) + 1
val step = (lMax - lMin) / steps

val ranges = (lMin to(end = lMax, step = step)).toList
val reversedRanges = ranges.reverse

val sideRangeLength = ranges.length / 2
val oddSideRangeLength = ranges.length % 2 != 0
val numberOfElementsToPow = if (!oddSideRangeLength) sideRangeLength - 1 else sideRangeLength

val lPowered = l.zipWithIndex.map { case (e, i) =>
  if (i <= numberOfElementsToPow) e.pow(i + 1)
  else if ((l.length - numberOfElementsToPow) <= i) e.pow((i - numberOfElementsToPow) + 1)
  else e
}

/* Group ranges so elements of the metric list can be compared */
val groupedRanges = ranges.grouped(2).zipWithIndex.map { case (e, i) => i -> e }.toMap

val rangesMap = if (groupedRanges.last._2.length == 1)
  groupedRanges.+(
    (groupedRanges.size - 1) ->
      List(groupedRanges.last._2.head, lMax)
  )
else groupedRanges

val lGeometricGrouped = l.groupBy { e =>
println(e)
  (rangesMap.find { case (k, v) =>
    v.head <= e && v.last > e
  }).get._1
}


lPowered.fold(BigDecimal(1))(_ + _) / lPowered.length








