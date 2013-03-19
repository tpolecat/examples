package eg

import scalaz.State   // the State[S,A] type itself
import scalaz.State._ // primitives like get and put are here
 
object StateExample extends App {
 
  // An action that produces a String
  val action1: State[Int, String] =
    for {
      n <- get
      _ <- put(n + 1)
    } yield "initial state was %d".format(n)
 
  // Another action that uses the first one
  val action2: State[Int, String] = for {
    s <- action1
    _ <- modify[Int](_ * 3)
  } yield "first result was: " + s
 
  println(action2(3))       // Get the final state and the result
  println(action2.eval(3))  // Or just the result
   
}
