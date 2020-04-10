package scheduler_model.sequence_split

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait SequenceSplitStrategy {

  def apply(seq: Seq[Double]): SplitResults

}

case class SplitResult(index: Int, seq: Seq[Double])

case class SplitResults(sourceSeq: Seq[Double], consecutiveValue: Double, results: Seq[SplitResult])

