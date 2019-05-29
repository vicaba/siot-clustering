package algebra

object MyDenseVector {
  def point[X: Numeric](s: Seq[X]): MyDenseVector[X] = new MyDenseVector[X](s)
}

class MyDenseVector[X: Numeric](private val s: Seq[X]) {

  def +(s2: MyDenseVector[X]): MyDenseVector[X] =
    sum(s2)

  def -(s2: MyDenseVector[X]): MyDenseVector[X] =
    subtract(s2)

  def sum(s2: MyDenseVector[X]): MyDenseVector[X] =
    MyDenseVector.point(SeqOps.sum(Seq(s ++ s2.s)))

  def subtract(s2: MyDenseVector[X]): MyDenseVector[X] =
    MyDenseVector.point(SeqOps.substract(Seq(s ++ s2.s)))

}
