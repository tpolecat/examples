package eg

object FBounds {

  // This pattern allows method signatures to mention the "current" type.
  // Not sure if we can improve on this.
  
  trait A[S <: A[S]] { this: S =>
    def foo:S
  }
  
  trait Foo extends A[Foo]
  trait Bar extends A[Bar]

  val f:Foo = (??? : Foo).foo
  val b:Bar = (??? : Bar).foo
  
}

