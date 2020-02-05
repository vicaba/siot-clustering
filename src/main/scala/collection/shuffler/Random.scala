package collection.shuffler

import scala.collection.generic.CanBuildFrom

object Random {
  def apply(random: util.Random): Random = new Random(random)
}

class Random(random: scala.util.Random) extends Shuffler {
  override def apply[T, CC[X] <: TraversableOnce[X]](xs: CC[T])(implicit bf: CanBuildFrom[CC[T], T, CC[T]]): CC[T] =
    random.shuffle(xs)
}
