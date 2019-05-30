package collection

object CollecctionHelper {
  def mutableSetOf[A](s: TraversableOnce[A]): scala.collection.mutable.Set[A] =
    new scala.collection.mutable.HashSet[A]() ++= s
}
