package types.ops
import breeze.linalg.norm
import types.mutable.Cluster
import types.Type
import types.Types.SyntheticDataType
import types.immutable.Point

object MirrorImage {

  trait FindMirror[T] {

    implicit def apply(origin: T, center: SyntheticDataType): SyntheticDataType

  }

  trait DistanceFunc[T] {

    implicit def apply(e1: T, e2: T): Double

    implicit def apply(e1: T, e2: SyntheticDataType): Double

  }

  trait Mirrored[T] {

    implicit def findMirror: FindMirror[T]

    implicit def distance: DistanceFunc[T]

  }

  implicit object MirroredType extends Mirrored[Type] {
    override def findMirror: FindMirror[Type] = new FindMirror[Type] {
      override def apply(origin: Type, center: SyntheticDataType): SyntheticDataType =
        (2.0 * center) - origin.syntheticValue
    }

    override implicit def distance: DistanceFunc[Type] = new DistanceFunc[Type] {
      override def apply(e1: Type, e2: Type): Double           = norm(e2.syntheticValue - e1.syntheticValue, 2)
      override def apply(e1: Type, e2: SyntheticDataType): Double = norm(e2 - e1.syntheticValue, 2)
    }

  }

  implicit object MirroredCluster extends Mirrored[Cluster] {
    override def findMirror: FindMirror[Cluster] = new FindMirror[Cluster] {
      override def apply(origin: Cluster, center: SyntheticDataType): SyntheticDataType =
        (2.0 * center) - origin.syntheticValue
    }

    override implicit def distance: DistanceFunc[Cluster] = new DistanceFunc[Cluster] {
      override def apply(e1: Cluster, e2: Cluster): Double           = norm(e2.syntheticValue - e1.syntheticValue, 2)
      override def apply(e1: Cluster, e2: SyntheticDataType): Double = norm(e2 - e1.syntheticValue, 2)
    }

  }

  implicit object MirroredPoint extends Mirrored[Point] {
    override def findMirror: FindMirror[Point] = new FindMirror[Point] {
      override def apply(origin: Point, center: SyntheticDataType): SyntheticDataType =
        (2.0 * center) - origin.syntheticValue
    }

    override implicit def distance: DistanceFunc[Point] = new DistanceFunc[Point] {
      override def apply(e1: Point, e2: Point): Double           = norm(e2.centroid - e1.centroid, 2)
      override def apply(e1: Point, e2: SyntheticDataType): Double = norm(e2 - e1.centroid, 2)
    }

  }

  /**
    * Given two points A and B, finds C as the mirror image of point A reflected into the plane perpendicular to line AB
    * and situated at distance norm(AB, 2) from A
    *
    * @param a the point to find the mirror image of
    * @param c the point where a perpendicular plane to line AB passes through
    * @return the mirror image
    */
  def findMirror[T: Mirrored](a: T, c: SyntheticDataType): SyntheticDataType =
    implicitly[Mirrored[T]].findMirror(a, c)

  def findClosestMirrors[T: Mirrored](origin: T,
                                      center: SyntheticDataType,
                                      points: IndexedSeq[T]): IndexedSeq[(Double, T)] = {

    val mirrored = implicitly[Mirrored[T]]

    val idealMirror = mirrored.findMirror(origin, center)

    points.zipWithIndex
      .map {
        case (point, idx) =>
          (mirrored.distance(point, idealMirror), point)
      }
      .sortBy(_._1)
  }

  def findClosestMirror[T: Mirrored](origin: T, center: SyntheticDataType, points: IndexedSeq[T]): Option[T] =
    findClosestMirrors(origin, center, points).headOption.map(_._2)

}
