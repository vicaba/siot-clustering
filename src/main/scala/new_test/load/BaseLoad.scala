package new_test.load

import new_test.load.Load._

abstract class BaseLoad extends SingleLoad {

  ensureCorrectCreation()

  override def startPositionInTime: LoadId = 0

}
