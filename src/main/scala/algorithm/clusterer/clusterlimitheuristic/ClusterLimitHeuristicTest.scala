package algorithm.clusterer.clusterlimitheuristic

trait ClusterLimitHeuristicTest {

  type Context

  def next(context: this.Context): this.Context

  def isFinished(context: this.Context): Boolean

}

class ElementLimitHeuristicTest(val maxElements: Int) extends ClusterLimitHeuristicTest {

  class ElementLimitHeuristicContext(val currentElements: Int)

  override type Context = ElementLimitHeuristicContext

  override def isFinished(context: this.Context): Boolean = context.currentElements == maxElements

  override def next(context: this.Context): this.Context =
    new ElementLimitHeuristicContext(context.currentElements + 1)
}

class MaxEnergyHeuristicTest
