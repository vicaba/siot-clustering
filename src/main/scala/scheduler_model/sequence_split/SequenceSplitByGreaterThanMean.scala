package scheduler_model.sequence_split

import breeze.numerics.{floor, round}

/**
  *
  * @param greaterThanXOfMean x <- [0.0, 1.0]
  */
class SequenceSplitByGreaterThanMean(greaterThanXOfMean: Double) extends SequenceSplitStrategy {

  override def apply(seq: Seq[Double]): SplitResults = {

    val (normalizedSeq, mean) = normalizeAndGetConsecutiveValue(seq)

    val consecutiveValue: Seq[Double] => Double = _ => {
      mean
    }
    new SequenceSplitByConsecutiveElements(consecutiveValue).apply(normalizedSeq)
  }

  def normalize(seq: Seq[Double]): Seq[Double] = normalizeAndGetConsecutiveValue(seq)._1

  private def normalizeAndGetConsecutiveValue(seq: Seq[Double]): (Seq[Double], Double) = {
    val mean = seq.sum / seq.size
    val threshold = mean + mean * greaterThanXOfMean
    var means = 0
    var acc = 0.0
    val temporaryNormalizedSeq = seq.map { e =>
      if (e <= threshold) {
        means = means + 1
        mean
      }
      else {
        acc = acc + e
        e
      }
    }
    // recompute consecutive value
    val newMean = mean - (temporaryNormalizedSeq.sum - seq.sum) / means
    (temporaryNormalizedSeq.map(e => if (e == mean) newMean else e), newMean)
  }

}
