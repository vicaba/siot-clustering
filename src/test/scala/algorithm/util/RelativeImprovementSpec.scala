package algorithm.util

import org.scalatest.Matchers._
import org.scalatest._

class RelativeImprovementSpec extends FeatureSpec with GivenWhenThen {

/*  feature("RelativeImprivement.isStuck works as expected") {

    scenario("RelativeImprivement.isStuck works as expected") {

      Given("some points")

      val points = globalPoints

      And("a cluster with those points")

      val cluster = globalCluster.deepCopy()

      When("asked to reschedule one point")

      val oldCluster = cluster.deepCopy()

      val scheduleResult = rescheduleTimes(11, cluster, metric)

      Then("the cluster improves distanceFunction")
      val originalCompatibility = metric(oldCluster)
      val betterCompatibility   = metric(scheduleResult.head.cluster)

      betterCompatibility should be < originalCompatibility

    }
  }*/

  List(1.2806637677052397,
    1.280680083940264,
    1.280664059866334,
    1.2806803138619798,
    1.2806643498224386,
    1.2806805420517053,
    1.2806646375984225,
    1.2806807685289376,
    1.2806649232187808,
    1.2806809933128822)

}
