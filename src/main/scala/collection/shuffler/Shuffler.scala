package collection.shuffler

import scala.collection.generic.CanBuildFrom

trait Shuffler {
  def apply[T, CC[X] <: TraversableOnce[X]](xs: CC[T])(implicit bf: CanBuildFrom[CC[T], T, CC[T]]): CC[T]
}

