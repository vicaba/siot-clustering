package benchmark

import breeze.linalg.DenseVector
import org.scalameter.Warmer
import org.scalameter.withWarmer
import org.scalameter.api._
import org.scalameter.picklers.Implicits._

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import types.clusterer
import types.clusterer.DataTypeMetadata.SyntheticDataType

import scala.annotation.tailrec
import scala.collection.immutable

class DenseVectorVsVectorBenchmark extends FlatSpec {

  val HowMany = 500000

  @tailrec
  final def sumVectors(remaining: List[SyntheticDataType], accum: DenseVector[Double]): DenseVector[Double] =
    remaining match {
      case e :: tail => sumVectors(tail, accum + e)
      case Nil       => accum
    }

  def generate[E](howMany: Int, gen: () => E): List[E] =
    (for (_ <- 0 until howMany) yield gen()).toList

  val denseVectors: List[DenseVector[Double]] = generate(HowMany, () => DenseVector(List.fill(48)(4.0): _*))
  val scalaVectors: List[Vector[Double]] = generate(HowMany, () => Vector.fill(48)(4.0))


  "a" should "a" in {
    val executionTimeOfDenseVectors = withWarmer(new Warmer.Default) measure {
      sumVectors(denseVectors.tail, denseVectors.head)
    }

    val executionTimeOfScalaVectors = withWarmer(new Warmer.Default) measure {
      algebra.SeqOps.sum(scalaVectors)
    }

    executionTimeOfDenseVectors.value should be < executionTimeOfScalaVectors.value

    info("execution time of dense vector: " + executionTimeOfDenseVectors.value + " " + executionTimeOfDenseVectors.units)
    info("execution time of scala vector: " + executionTimeOfScalaVectors.value + " " + executionTimeOfScalaVectors.units)
  }


}
