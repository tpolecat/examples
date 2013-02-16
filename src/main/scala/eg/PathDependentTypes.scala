package eg

object D extends App {
  trait T { type C }
  class X[A <: T](t: A) { def f(c: t.C) = 1 }
  val t = new T { type C = Int }
  new X(t).f(1)
}