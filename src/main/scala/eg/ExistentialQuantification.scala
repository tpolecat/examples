package eg

import language.existentials

object ExistentialQuantification extends App {

  case class Foo[A](a: A)

  def f(as: (Foo[A], A) forSome { type A }*) = 1

  f((Foo(1), 2), (Foo("bar"), "baz"))
//f((Foo(1), 2), (Foo("bar"), "baz"), (Foo("qux"), 3)) // doesn't compile

}

