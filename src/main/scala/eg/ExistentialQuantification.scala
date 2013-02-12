package eg

import language.existentials
import language.higherKinds

object ExistentialQuantification extends App {

  case class Foo[A](a: A)

  // Each pair must have the same A in both slots, but the pairs can differ
  // one to the next.
  def f(as: (Foo[A], A) forSome { type A }*) = 1

  // Thus
  f((Foo(1), 2), (Foo("bar"), "baz"))
  
  // This doesn't compile (but will if Foo is covariant in A)
  // f((Foo(1), 2), (Foo("bar"), "baz"), (Foo("qux"), 3)) 

  // Works with Set, which is invariant
  val s:Set[(Foo[A], A) forSome { type A }] = Set((Foo(1), 2), (Foo("bar"), "baz"))
  // val s:Set[(Foo[A], A) forSome { type A }] = Set((Foo(1), 2), (Foo("bar"), "baz"))

  // Doesn't work at all with List, which is covariant
  // val x:List[(Foo[A], A) forSome { type A }] = List((Foo(1), 2), (Foo("bar"), "baz"))
  
}

