package test

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait SubTaskSplitter {

  def split(s: SpanSlotFlexibleLoad, splitStrategy: SequenceSplitStrategy[Double]): Seq[SpanSlotFlexibleLoadSubTask]

}

case class SplitResult[E](index: Int, seq: Seq[E])

trait SequenceSplitStrategy[E] {

  def apply(seq: Seq[E]): Seq[SplitResult[E]]

}

object SequenceSplitByConsecutiveElements {

  def apply[E](consecutiveValue: Seq[E] => E): SequenceSplitByConsecutiveElements[E] =
    new SequenceSplitByConsecutiveElements(consecutiveValue)

  def withConsecutiveValueAsTheHighestCount[E]: SequenceSplitByConsecutiveElements[E] = {
    val consecutiveValue: Seq[E] => E = seq => {
      seq
        .foldLeft(Map.empty[E, Int].withDefaultValue(0)) {
          case (m, v) => m.updated(v, m(v) + 1)
        }
        .maxBy(_._2)
        ._1
    }
    SequenceSplitByConsecutiveElements(consecutiveValue)
  }

}

class SequenceSplitByConsecutiveElements[E](val consecutiveValue: Seq[E] => E) extends SequenceSplitStrategy[E] {

  override def apply(seq: Seq[E]): Seq[SplitResult[E]] =
    splitSequenceBySequenceOfElements(seq, consecutiveValue(seq)).map(r => SplitResult(r._1, r._2))

  private def splitSequenceBySequenceOfElements(seq: Seq[E], consecutiveValue: E): Seq[(Int, Seq[E])] = {

    class Extracted(val extractedSeq: Seq[E], val remainingSeq: Seq[E])
    object Extracted {
      def apply(extractedSeq: Seq[E], remainingSeq: Seq[E]): Extracted = new Extracted(extractedSeq, remainingSeq)
    }

    @tailrec
    def _extractSequenceOfElements(_seq: Seq[E],
                                   accum: scala.collection.mutable.ListBuffer[E] = new ListBuffer[E],
                                   comparator: (E, E) => Boolean): Extracted = {
      if (_seq.isEmpty) return Extracted(accum, _seq)
      if (comparator(_seq.head, consecutiveValue))
        _extractSequenceOfElements(_seq.tail, accum += _seq.head, comparator)
      else Extracted(accum, _seq)
    }

    @tailrec
    def _splitSequenceBySequenceOfElements(index: Int,
                                           remainingSeq: Seq[E],
                                           accum: Seq[(Int, Seq[E])]): Seq[(Int, Seq[E])] = {
      if (remainingSeq.isEmpty) return accum
      if (remainingSeq.head == consecutiveValue) {
        val extracted = _extractSequenceOfElements(remainingSeq, new ListBuffer[E], (e1, e2) => e1 == e2)
        _splitSequenceBySequenceOfElements(index + extracted.extractedSeq.length, extracted.remainingSeq, accum)
      } else {
        val extracted = _extractSequenceOfElements(remainingSeq, new ListBuffer[E], (e1, e2) => e1 != e2)
        _splitSequenceBySequenceOfElements(index + extracted.extractedSeq.length,
                                           extracted.remainingSeq,
                                           (index, extracted.extractedSeq) +: accum)
      }

    }

    _splitSequenceBySequenceOfElements(index = 0, seq, Seq())

  }
}
