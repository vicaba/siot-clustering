package types.ops
import scala.collection.mutable

object SetOps {

  implicit class ImmutableSetOps[T](s: Set[T]) {

    def -/+(elem: T): Set[T] =
      (s - elem) + elem

    def --/++(elem: Traversable[T]): Set[T] =
      (s -- elem) ++ elem

  }

  implicit class MutableSetOps[T](s: mutable.Set[T]) {

    def -/+=(e: T): mutable.Set[T] = {
      (s -= e) += e
    }

    def --/++=(es: Traversable[T]): mutable.Set[T] = {
      (s --= es) ++= es
    }

  }

}
