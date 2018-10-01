package algorithm.algorithms
import algorithm.algorithms.BruteAlgorithm.{Step1, Step2, Steps, logger}
import algorithm.clusterer.BruteClusterer
import algorithm.scheduler.ClusterRescheduler
import algorithm.scheduler.ClusterRescheduler.PointChanged
import types.Cluster

trait GenAlgorithm {

  type Step1

  type Step2

  type ClustererSettings

  type ReschedulerSettings

  case class StepT[Settings](settings: Settings, clusters: List[Cluster])

  case class Steps(_1: StepT[ClustererSettings], _2: StepT[ReschedulerSettings])

/*  def apply(clustererSettings: BruteClusterer.Settings, reschedulerSettings: ClusterRescheduler.Settings): Steps = {

    logger.info("clusterer")

    val clustererResult = BruteClusterer(clustererSettings)

    logger.info("rescheduler")

    val reschedulerResult = clustererResult.map(ClusterRescheduler(_, reschedulerSettings)._1).toList

    logger.info("end")

    Steps(
      _1 = Step1(clustererSettings, clustererResult, clustererSettings.metric.aggregateOf(clustererResult)),
      _2 = Step2(reschedulerSettings, reschedulerResult, reschedulerSettings.metric.aggregateOf(reschedulerResult))
    )

  }*/

  def apply(
      clusterer: () => (ClustererSettings, List[Cluster]),
      rescheduler: List[Cluster] => (ReschedulerSettings, List[(Cluster, List[PointChanged])]))
    : Steps = {

    logger.info("clusterer")

    val clustererResult = clusterer()

    logger.info("rescheduler")

    val reschedulerResult = rescheduler(clustererResult._2)

    logger.info("end")

    Steps(
      _1 = StepT(clustererResult._1, clustererResult._2),
      _2 = StepT(reschedulerResult._1, reschedulerResult._2.map(_._1))
    )

  }

}
