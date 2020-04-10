package scheduler_model.sequence_split

object SequenceSplitByZero extends SequenceSplitStrategy {

  override def apply(seq: Seq[Double]): SplitResults =
    new SequenceSplitByConsecutiveElements(_ => 0.0).apply(seq)

}
