package algebra

import scala.collection.{AbstractSeq, IndexedSeqLike}
import scala.collection.generic.GenericTraversableTemplate
import scala.collection.immutable.Vector

object VectorOps {

  implicit def vectorToMyDenseVector[X: Numeric](v: Vector[X]): MyDenseVector[X] = MyDenseVector.point(v)

}

object MyDenseVector {
  def point[X: Numeric](s: Vector[X]): MyDenseVector[X] = new MyDenseVector[X](s)
}

class MyDenseVector[X: Numeric](val underlying: Vector[X]) {

  def sumVec(s2: MyDenseVector[X]): MyDenseVector[X] =
    MyDenseVector.point(SeqOps.sum(Seq(underlying ++ s2.underlying)))

  def subtractVec(s2: MyDenseVector[X]): MyDenseVector[X] =
    MyDenseVector.point(SeqOps.substract(Seq(underlying ++ s2.underlying)))

}
