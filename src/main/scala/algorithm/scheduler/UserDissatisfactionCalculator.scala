package algorithm.scheduler

import scheduler_model.load.{AccumulatedLoad, FlexibleLoad, FlexibleLoadSubTask, Load}

object UserDissatisfactionCalculator {

  val sortByPredicate: Load => (Int, Int, String) = f => (f.id, f.group, f.label)

  def listOfAccumulatedLoadsDissatisfaction(accumulatedLoadList1: List[AccumulatedLoad], accumulatedLoadList2: List[AccumulatedLoad]): Int = {
    UserDissatisfactionCalculator.flexibleLoadDissatisfaction(
      accumulatedLoadList1.flatMap(_.flexibleLoads),
      accumulatedLoadList2.flatMap(_.flexibleLoads)
    )
  }

  def accumulatedLoadsDissatisfaction(accumulatedLoad1: AccumulatedLoad, accumulatedLoad2: AccumulatedLoad): Int =
    flexibleLoadDissatisfaction(accumulatedLoad1.flexibleLoads.toList, accumulatedLoad2.flexibleLoads.toList)

  def flexibleLoadDissatisfaction(flexibleLoads: List[FlexibleLoad], flexibleLoads2: List[FlexibleLoad]): Int = {
    (flexibleLoads.filter(_.isInstanceOf[FlexibleLoadSubTask]).sortBy(sortByPredicate)
      zip flexibleLoads2.filter(_.isInstanceOf[FlexibleLoadSubTask]).sortBy(sortByPredicate)).map {
      case (fl1, fl2) =>
        flexibleLoadSubTaskDissatisfaction(fl1.asInstanceOf[FlexibleLoadSubTask], fl2.asInstanceOf[FlexibleLoadSubTask])
    }.sum
  }

  def flexibleLoadSubTaskDissatisfaction(flexibleLoadSubTask: FlexibleLoadSubTask,
                                         flexibleLoadSubTask2: FlexibleLoadSubTask): Int = {
    Math.abs(flexibleLoadSubTask.startPositionInTime - flexibleLoadSubTask2.startPositionInTime)
  }

}
