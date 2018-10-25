package types.ops
import scala.collection.mutable

object SetOps {

  implicit class ImmutableSetOps[T](s: Set[T]) {
    def +=(elem: T): Set[T] = {
      (s - elem) + elem
    }
  }

  implicit class MutableSetOps[T](s: mutable.Set[T]) {
    def +=(elem: T): mutable.Set[T] = {
      (s - elem) + elem
    }
  }

}
