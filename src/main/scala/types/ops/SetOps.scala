package types.ops
import scala.collection.mutable

object SetOps {

  implicit class ImmutableSetOps[T](s: Set[T]) {

    def -/+(elem: T): Set[T] =
      (s - elem) + elem

    def --/++(elem: TraversableOnce[T]): Set[T] =
      (s -- elem) ++ elem

  }

}
