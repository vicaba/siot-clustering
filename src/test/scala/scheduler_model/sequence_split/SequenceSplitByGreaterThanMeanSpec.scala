package scheduler_model.sequence_split

import breeze.linalg.DenseVector
import org.scalatest.FeatureSpec
import org.scalatest.Matchers._
import scheduler_model.load.{AccumulatedLoad, FixedLoad, FlexibleLoad, LoadOps}

class SequenceSplitByGreaterThanMeanSpec extends FeatureSpec {

  scenario("When transforming a sequence, the sum of elements is the same on both") {

    val seq           = Seq(2.0, 3.0)
    val splitStrategy = new SequenceSplitByGreaterThanMean(0.5)
    val res           = splitStrategy.normalize(seq)
    seq.sum shouldBe res.sum

  }

  scenario(
    "When transforming an accumulated load with flexible loads to flexibleloads with subtascs, the sum of elements is the same on both") {

    val users: List[AccumulatedLoad] = List(
      AccumulatedLoad.AutoSpanFromLoads(100,
                                        100,
                                        "100",
                                        List(
                                          FixedLoad(101, 101, "101", DenseVector(3, 3, 3, 0, 1, 2)),
                                          FlexibleLoad(151, 151, "151", 0, DenseVector(2, 3))
                                        )),
      AccumulatedLoad.AutoSpanFromLoads(200,
                                        200,
                                        "200",
                                        List(
                                          FixedLoad(201, 201, "201", DenseVector(3, 1, 0, 5, 2, 1)),
                                          FlexibleLoad(251, 251, "251", 0, DenseVector(5, 3))
                                        )),
      AccumulatedLoad.AutoSpanFromLoads(300,
                                        300,
                                        "300",
                                        List(
                                          FixedLoad(301, 301, "301", DenseVector(2, 0, 4, 5, 3, 1)),
                                          FlexibleLoad(351, 351, "351", 0, DenseVector(4, 4))
                                        ))
    )

    val newUsers = LoadOps.copy(users)

    newUsers
      .asInstanceOf[List[AccumulatedLoad]]
      .foreach(AccumulatedLoad.Mutate
        .splitFlexibleLoadsIntoTasksAndPrepareForSchedulerAlgorithm(_, new SequenceSplitByGreaterThanMean(0.5)))

    AccumulatedLoad.AutoSpanFromLoads(-1, -1, "-1", newUsers).totalEnergy shouldBe AccumulatedLoad
      .AutoSpanFromLoads(-1, -1, "-1", users)
      .totalEnergy

  }

}
