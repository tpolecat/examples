package eg

import language.higherKinds
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag
import scala.util.Sorting

object CanBuildFromExample {

  // thanks to non/d_m and tixxit
  // https://github.com/non/spire/blob/2.10.0/core/src/main/scala/spire/package.scala#L350
  def foo[A, CC[A] <: Iterable[A]](as: CC[A])(implicit ct: ClassTag[A], cbf: CanBuildFrom[CC[A], A, CC[A]]): CC[A] = {
    as.toArray.to[CC] // convert to an array and back to original type
  }

  val x: List[Int] = foo(List(1, 2, 3))

}