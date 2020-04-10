package scheduler_model.sequence_split

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object SequenceSplitByConsecutiveElements {

  def apply(consecutiveValue: Seq[Double] => Double): SequenceSplitByConsecutiveElements =
    new SequenceSplitByConsecutiveElements(consecutiveValue)

  def withConsecutiveValueAsTheHighestCountAndConsecutiveValueBelowAverage: SequenceSplitByConsecutiveElements = {
    val consecutiveValue: Seq[Double] => Double = seq => {

      /*      val average = seq.foldLeft(0.0) {
              case (acc, v) => acc + v
            } / seq.size*/

      val consecutiveValues = seq
        .foldLeft(Map.empty[Double, Int].withDefaultValue(0)) {
          case (m, v) => m.updated(v, m(v) + 1)
        }
        .toList.sortWith { case (x, y) => x._2 > y._2}

      //consecutiveValues.filter(_._2 == consecutiveValues.maxBy(_._2)._2).collectFirst {case (elem, _) if elem < average => elem}.getOrElse(consecutiveValues.minBy(_._1)._1)
      consecutiveValues.minBy(_._1)._1
    }
    SequenceSplitByConsecutiveElements(consecutiveValue)
  }

  def withConsecutiveValueAsTheHighestCount: SequenceSplitByConsecutiveElements = {
    val consecutiveValue: Seq[Double] => Double = seq => {
      seq
        .foldLeft(Map.empty[Double, Int].withDefaultValue(0)) {
          case (m, v) => m.updated(v, m(v) + 1)
        }
        .maxBy(_._2)
        ._1
    }
    SequenceSplitByConsecutiveElements(consecutiveValue)
  }

}

class SequenceSplitByConsecutiveElements(val consecutiveValue: Seq[Double] => Double) extends SequenceSplitStrategy {

  override def apply(seq: Seq[Double]): SplitResults = {
    val _consecutiveValue = consecutiveValue(seq)
    SplitResults(seq,
      _consecutiveValue,
      splitSequenceBySequenceOfElements(seq, _consecutiveValue).map(r => SplitResult(r._1, r._2)))
  }

  private def splitSequenceBySequenceOfElements(seq: Seq[Double], consecutiveValue: Double): Seq[(Int, Seq[Double])] = {

    class Extracted(val extractedSeq: Seq[Double], val remainingSeq: Seq[Double])
    object Extracted {
      def apply(extractedSeq: Seq[Double], remainingSeq: Seq[Double]): Extracted = new Extracted(extractedSeq, remainingSeq)
    }

    @tailrec
    def _extractSequenceOfElements(_seq: Seq[Double],
      accum: scala.collection.mutable.ListBuffer[Double] = new ListBuffer[Double],
      comparator: (Double, Double) => Boolean): Extracted = {
      if (_seq.isEmpty) return Extracted(accum, _seq)
      if (comparator(_seq.head, consecutiveValue))
        _extractSequenceOfElements(_seq.tail, accum += _seq.head, comparator)
      else Extracted(accum, _seq)
    }

    @tailrec
    def _splitSequenceBySequenceOfElements(index: Int,
      remainingSeq: Seq[Double],
      accum: Seq[(Int, Seq[Double])]): Seq[(Int, Seq[Double])] = {
      if (remainingSeq.isEmpty) return accum
      if (remainingSeq.head == consecutiveValue) {
        val extracted = _extractSequenceOfElements(remainingSeq, new ListBuffer[Double], (e1, e2) => e1 == e2)
        _splitSequenceBySequenceOfElements(index + extracted.extractedSeq.length, extracted.remainingSeq, accum)
      } else {
        val extracted = _extractSequenceOfElements(remainingSeq, new ListBuffer[Double], (e1, e2) => e1 != e2)
        _splitSequenceBySequenceOfElements(index + extracted.extractedSeq.length,
          extracted.remainingSeq,
          (index, extracted.extractedSeq) +: accum)
      }

    }

    _splitSequenceBySequenceOfElements(index = 0, seq, Seq())

  }
}
