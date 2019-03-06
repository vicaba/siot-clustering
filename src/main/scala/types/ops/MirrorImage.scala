package types.ops
import breeze.linalg.norm
import types.mutable.Cluster
import types.Type
import types.DataTypeMetadata.SyntheticDataType
import types.immutable.Point

object MirrorImage {

  /**
    * Typeclass that provides the mirror for type T
    *
    * @tparam T
    */
  trait FindMirror[T] {

    /**
      * Given an origin and a center, find the mirror image of origin from that center
      *
      * @param origin
      * @param center
      * @return
      */
    implicit def apply(origin: T, center: SyntheticDataType): SyntheticDataType

  }

  /**
    * Typeclass that provides distance between types T
    *
    * @tparam T
    */
  trait DistanceFunc[T] {

    /**
      * Calculates distance between e1 and e2
      *
      * @param e1
      * @param e2
      * @return the distance between e1 and e2
      */
    implicit def apply(e1: T, e2: T): Double

    /**
      * Calcuates distance between e1 and e2
      *
      * @param e1
      * @param e2
      * @return the distance between e1 and e2
      */
    implicit def apply(e1: T, e2: SyntheticDataType): Double

  }

  /**
  * Typeclass that provides distance and mirror typeclasses
    * @tparam T
    */
  trait Mirrored[T] {

    implicit def findMirror: FindMirror[T]

    implicit def distance: DistanceFunc[T]

  }

  implicit object MirroredSyntheticDataType extends Mirrored[SyntheticDataType] {
    override implicit def findMirror: FindMirror[SyntheticDataType] = new FindMirror[SyntheticDataType] {
      override implicit def apply(origin: SyntheticDataType, center: SyntheticDataType): SyntheticDataType =
        (2.0 * center) - origin
    }

    override implicit def distance: DistanceFunc[SyntheticDataType] = new DistanceFunc[SyntheticDataType] {
      override implicit def apply(e1: SyntheticDataType, e2: SyntheticDataType): Double = norm(e2 - e1, 2)
    }
  }

  implicit object MirroredType extends Mirrored[Type] {
    override def findMirror: FindMirror[Type] =
      (origin: Type, center: SyntheticDataType) => (2.0 * center) - origin.syntheticValue

    override implicit def distance: DistanceFunc[Type] = new DistanceFunc[Type] {
      override def apply(e1: Type, e2: Type): Double              = norm(e2.syntheticValue - e1.syntheticValue, 2)
      override def apply(e1: Type, e2: SyntheticDataType): Double = norm(e2 - e1.syntheticValue, 2)
    }

  }

  implicit object MirroredCluster extends Mirrored[Cluster] {
    override def findMirror: FindMirror[Cluster] =
      (origin: Cluster, center: SyntheticDataType) => (2.0 * center) - origin.syntheticValue

    override implicit def distance: DistanceFunc[Cluster] = new DistanceFunc[Cluster] {
      override def apply(e1: Cluster, e2: Cluster): Double           = norm(e2.syntheticValue - e1.syntheticValue, 2)
      override def apply(e1: Cluster, e2: SyntheticDataType): Double = norm(e2 - e1.syntheticValue, 2)
    }

  }

  implicit object MirroredPoint extends Mirrored[Point] {
    override def findMirror: FindMirror[Point] =
      (origin: Point, center: SyntheticDataType) => (2.0 * center) - origin.syntheticValue

    override implicit def distance: DistanceFunc[Point] = new DistanceFunc[Point] {
      override def apply(e1: Point, e2: Point): Double             = norm(e2.centroid - e1.centroid, 2)
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

  /**
    * Given two points origin and center, orders points from the the closest mirror image of point origin
    * reflected into the plane perpendicular to line origin-center and situated at distance norm(origin-center, 2)
    * from origin
    * @param origin the point to find the mirror image of
    * @param center the point where a perpendicular plane to line AB passes through
    * @param points the set of points to search for the closest mirror image
    * @tparam T the type that provides a typeclass to find the mirror image from it
    * @return A list of distances and points ordered by its closeness in incremental order (shortest distance first)
    */
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

  /**
  * The same as [[findClosestMirrors()]] but only returns the closest
    */
  def findClosestMirror[T: Mirrored](origin: T, center: SyntheticDataType, points: IndexedSeq[T]): Option[T] =
    findClosestMirrors(origin, center, points).headOption.map(_._2)

}
