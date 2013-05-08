package eg

import language.higherKinds
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag
import scala.util.Sorting

// SEE ALSO the ZipMany example, which is much more comprehensive

object CanBuildFromExample extends App {
  
  // Turn something into an array and then back into a collection of the same type
  def foo[A: ClassTag, C[A] <: Traversable[A]](as: C[A])(implicit cbf: CanBuildFrom[C[A], A, C[A]]): C[A] = {
    as.toArray.to[C] // convert to an array and back to original type
  }

  val x: List[Int] = foo(List(1, 2, 3))

  // Turn any traversable into an opaque traversable of the same type  
  def bar[A, C[A] <: Traversable[A]](as: C[A])(implicit cbf: CanBuildFrom[C[A], Any, C[Any]]):C[_] = {
    as.map("foo" + _ : Any).to[C]
  }

  println(bar(List(1,2,3)))
  println(bar(Vector(1,2,3)))
  println(bar(Set(1,2,3)))
  
}