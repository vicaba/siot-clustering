package test

import algorithm.clusterer.clusterlimitheuristic.{ClusterLimitHeuristic, MaxEnergyHeuristic}
import algorithm.clusterer.elementlocatorheuristic.ElementLocatorHeuristic
import types.DataTypeMetadata.SyntheticDataType
import types.mutable.Cluster

trait LimitHeuristic[Feed] {

  type F = Feed

  type Context

  def isFinished(context: this.Context): Boolean

  def next(context: this.Context, feed: Feed): this.Context

}

class LimitHeuristicImpl(val maxElements: Integer) extends LimitHeuristic[Integer] {

  case class ContextImpl(currentElements: F)

  override type Context = ContextImpl

  override def isFinished(context: this.Context): Boolean = context.currentElements == maxElements

  override def next(context: this.Context, feed: F): this.Context = {
    new ContextImpl(context.currentElements + feed)
  }
}

class LimitHeuristicImpl2(val maxElements: Integer) extends LimitHeuristic[Integer] {

  case class ContextImpl(currentElements: F)

  override type Context = ContextImpl

  override def isFinished(context: this.Context): Boolean = context.currentElements == maxElements

  override def next(context: this.Context, feed: F): this.Context = {
    new ContextImpl(context.currentElements - feed)
  }
}

object Main {

  def f(l: LimitHeuristic[Integer])(c: l.Context): String = {
    val newContext = l.next(c, 1)
    if (l.isFinished(newContext)) newContext.toString else f(l)(newContext)
  }

  def main(args: Array[String]): Unit = {

    val lh = new LimitHeuristicImpl(3)
    val lh2 = new LimitHeuristicImpl2(-3)

    println(f(lh)(new lh.ContextImpl(1)))

  }

  def clustersToClusterUntilHeuristic(
                                       cluster: Cluster,
                                       centroid: SyntheticDataType,
                                       freeClusters: IndexedSeq[Cluster],
                                       elementLocatorHeuristic: ElementLocatorHeuristic,
                                       clusterLimitHeuristic: LimitHeuristic[Cluster]): (Cluster, IndexedSeq[Cluster]) = {

    val e = elementLocatorHeuristic(cluster, centroid, freeClusters).head._2
    clusterLimitHeuristic.next(e)


  }

}
