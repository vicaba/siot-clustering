package types.ops

object SetOps {

  implicit class SetOps[T](s: Set[T]) {
    def +=(elem: T): Set[T] = {
      (s - elem) + elem
    }
  }

}
