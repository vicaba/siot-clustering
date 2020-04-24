package algorithm.scheduler

import scheduler_model.load.{FlexibleLoad, FlexibleLoadSubTask, FlexibleLoadSuperTask}

object UserDissatisfactionCalculator {

  def flexibleLoadDissatisfaction(flexibleLoads: List[FlexibleLoad], flexibleLoads2: List[FlexibleLoad]): Int = {
    val sortByPredicate: FlexibleLoad => (Int, Int, String) = f => (f.id, f.group, f.label)
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
