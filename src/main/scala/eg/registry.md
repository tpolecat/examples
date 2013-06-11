Killing the Registry Pattern
----------------------------

Yet another idea that falls into the category of "probably obvious, possibly wrong, but I think it's interesting."

In cases where you might have a registry in Java, you can instead have a statically-checked "registry" where the keys are vacuous types and the values are located via implicit search.

This is interesting because the type parameter is never used and the type argument need not be inhabited.

```scala

// A trait that we want a registry for, keyed on type parameter A
trait Greeter[A] {
  def sayHi(s:String): String
}

// Implementation for English
sealed trait English 
object English {
  implicit object EnglishGreeter extends Greeter[English] {
    def sayHi(s:String): String = s"Hello, $s!"
  }
}

// Implementation for Spanish
sealed trait Español 
object Español {
  implicit object GreeterEnEspañol extends Greeter[Español] {
    def sayHi(s:String): String = s"¡Hola, $s!"
  }
}

def greet[A](s:String)(implicit ev: Greeter[A]) {
  println(ev.sayHi(s))
}

greet[English]("Arthur")
greet[Español]("Arturo")

```


