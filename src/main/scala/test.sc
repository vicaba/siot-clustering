import breeze.linalg._

val cols = 4
val startPosition = 2
val span = 2

val baseVector = DenseVector.fill[Double](cols, 0.0)
for ((baseVectorIndex, vectorIndex) <- ((startPosition until cols) zip (0 until span)) yield {
}

val ms = startPosition until cols
val as = 0 until span

var sms = for ( (m, a) <- (ms zip as)) yield m