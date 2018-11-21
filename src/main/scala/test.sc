import scala.collection.mutable

def test1() = {

  trait Abstract {
    type ThisType <: Abstract
    def deepCopy(): ThisType
  }

  case class Concrete1(a: Int) extends Abstract {
    override type ThisType = Concrete1
    override def deepCopy(): ThisType = this.copy()

  }

  case class Concrete2(a: Int) extends Abstract {
    override type ThisType = Concrete2
    override def deepCopy(): ThisType = this.copy()

  }

  val set = new mutable.HashSet[Abstract]()

  set ++= List(Concrete1(1), Concrete2(2))

  val set2: mutable.Set[Abstract] = set.map(_.deepCopy())

}

def test2() = {

  trait Abstract {
    type ThisType
    def deepCopy(): ThisType
  }

  case class Concrete1(a: Int) extends Abstract {
    override type ThisType = Concrete1
    override def deepCopy(): ThisType = this.copy()

  }

  case class Concrete2(a: Int) extends Abstract {
    override type ThisType = Concrete2
    override def deepCopy(): ThisType = this.copy()

  }

  val set = new mutable.HashSet[Abstract]()

  set ++= List(Concrete1(1), Concrete2(2))

  val set2: mutable.Set[Abstract] = set.map(_.deepCopy())

}

def test3() = {


  trait Abstract[T <: Abstract[T]] {
    def deepCopy(): T
  }

  case class Concrete1(a: Int) extends Abstract[Concrete1] {
    override def deepCopy(): Concrete1 = this.copy()

  }

  case class Concrete2(a: Int) extends Abstract[Concrete2] {
    override def deepCopy(): Concrete2 = this.copy()

  }

  val set = new mutable.HashSet[Abstract[_]]()

  set ++= List(Concrete1(1), Concrete2(2))

  val set2: mutable.Set[Abstract[_]] = set.map(_.deepCopy())

}
