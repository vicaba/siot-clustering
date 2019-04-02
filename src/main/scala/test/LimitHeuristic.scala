package test

trait LimitHeuristic {

  type Context

  def apply(context: this.Context): (Boolean, this.Context)

}

class LimitHeuristicImpl extends LimitHeuristic {

  case class Context1(a: Int)

  override type Context = Context1

  override def apply(context: this.Context): (Boolean, this.Context) = {
    if (context.a == 3) (false, context) else (true, Context1(context.a + 1))
  }
}

class LimitHeuristicImpl2 extends LimitHeuristic {

  case class Context2()

  override type Context = Context2

  override def apply(context: this.Context): (Boolean, this.Context) = (true, Context2())
}

object Main {

  def f(l: LimitHeuristic)(c: l.Context): String = {
    val res = l.apply(c)
    if (res._1) f(l)(res._2) else res._2.toString
  }

  def main(args: Array[String]): Unit = {

    val lh = new LimitHeuristicImpl()
    val lh2 = new LimitHeuristicImpl2()

    println(f(lh)(lh.Context1(0)))

  }
}
