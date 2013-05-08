package eg

import language.higherKinds
import language.implicitConversions

// CREDIT - This excellent example was written by Ryan LeCompte @ryanlecompte 

object ZipMany {

  // Implementation
  import scala.collection._
  import scala.collection.generic.CanBuildFrom

  /**
   * RichIterable provides useful methods on top of Iterable instances.
   * See the following for an explanation on how CanBuildFrom is used here:
   * http://stackoverflow.com/questions/5410846/how-do-i-apply-the-enrich-my-library-pattern-to-scala-collections
   */
  class RichIterable[A, C[A] <: Iterable[A]](underlying: C[A]) {
    def zipMany(iterables: C[A]*)(implicit cbfcc: CanBuildFrom[C[A], C[A], C[C[A]]], cbfc: CanBuildFrom[C[A], A, C[A]]): C[C[A]] = {
      val its = underlying.iterator +: iterables.map { _.iterator }
      val finalBuilder = cbfcc()
      finalBuilder.sizeHint(iterables.map { _.size }.min)
      while (its.forall { _.hasNext }) {
        val builder = cbfc()
        builder ++= its.map { _.next() }
        finalBuilder += builder.result()
      }
      finalBuilder.result()
    }
  }

  /**
   * RichManyZippedIterable is a wrapper that knows how to reverse a zipMany operation.
   * The following "type evidence" (<:<) is required since we want to put bounds on a specific "instantiation" of
   * C[B[A]]. Thanks to @copumpkin for helping figure this part out.
   */
  class RichManyZippedIterable[A, B[_], C[_]](underlying: C[B[A]])(implicit x: B[A] <:< Iterable[A], y: C[B[A]] <:< Iterable[B[A]]) {
    def unzipMany(implicit cbfcc: CanBuildFrom[C[A], C[A], C[C[A]]], cbfc: CanBuildFrom[C[A], A, C[A]]): C[C[A]] = {
      require(underlying.map { _.size }.toSeq.distinct.size == 1, "subgroups must be the same length")
      val its = underlying.map { _.iterator }
      val finalBuilder = cbfcc()
      finalBuilder.sizeHint(underlying.size)
      while (its.forall { _.hasNext }) {
        val builder = cbfc()
        builder ++= its.map { _.next() }
        finalBuilder += builder.result()
      }
      finalBuilder.result()
    }
  }

  // implicit conversion methods here
  implicit def iterable2RichIterable[A, C[A] <: Iterable[A]](i: C[A]) = new RichIterable(i)
  implicit def zippedIterable2RichZippedIterable[A, B[_], C[_]](i: C[B[A]])(implicit x: B[A] <:< Iterable[A], y: C[B[A]] <:< Iterable[B[A]]) = new RichManyZippedIterable(i)

}

object ZipManyTest {
  import ZipMany._

  // Usage examples:
  val a = Vector(1, 2, 3).zipMany(Vector(1, 2, 3), Vector(5, 6, 7), Vector(8, 9, 10), Vector(11, 12, 13))
  Vector(Vector(1, 1, 5, 8, 11), Vector(2, 2, 6, 9, 12), Vector(3, 3, 7, 10, 13))

  a.unzipMany
  Vector(Vector(1, 2, 3), Vector(1, 2, 3), Vector(5, 6, 7), Vector(8, 9, 10), Vector(11, 12, 13))

}
