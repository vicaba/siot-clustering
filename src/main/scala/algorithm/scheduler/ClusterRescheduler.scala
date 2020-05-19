package algorithm.scheduler

import com.typesafe.scalalogging.Logger
import config.GlobalConfig
import scheduler_model.load.AccumulatedLoad

object ClusterRescheduler {

  val logger = Logger("Rescheduler")

  def apply(clustersAsAccumulatedLoad: List[AccumulatedLoad], settings: ReschedulerSettings): List[AccumulatedLoad] = {

    if (clustersAsAccumulatedLoad.isEmpty) return Nil

    GlobalConfig.instance.schedulerType match {
      case GlobalConfig.SchedulerType.Coordinated =>
        Scheduler.apply(clustersAsAccumulatedLoad,
          settings.metricTransformation,
          settings.userOrderings,
          settings.schedulerAlgorithmOrderings)
      case GlobalConfig.SchedulerType.UnCoordinated =>
        clustersAsAccumulatedLoad.par.flatMap { cluster =>
          Scheduler.apply(List(cluster),
            settings.metricTransformation,
            settings.userOrderings,
            settings.schedulerAlgorithmOrderings)
        }.toList
    }
  }

}
