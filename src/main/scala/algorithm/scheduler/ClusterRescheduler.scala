package algorithm.scheduler

import com.typesafe.scalalogging.Logger
import scheduler_model.load.AccumulatedLoad

object ClusterRescheduler {

  val logger = Logger("Rescheduler")

  def apply(clustersAsAccumulatedLoad: List[AccumulatedLoad], settings: ReschedulerSettings): List[AccumulatedLoad] = {

    if (clustersAsAccumulatedLoad.isEmpty) return Nil

    Scheduler.apply(clustersAsAccumulatedLoad,
                    settings.metricTransformation,
                    settings.userOrderings,
                    settings.schedulerAlgorithmOrderings)
  }

}
