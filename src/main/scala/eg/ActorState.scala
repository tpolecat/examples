package eg

import scalaz._
import scalaz.concurrent._
import scalaz.effect._
import scalaz.effect.stateTEffect.StateTMonadIO

import State._
import StateT._
import IO._

object ActorState extends App {

  abstract class StatefulActor[S, A](initialState: S) {

    type IOState[+A] = StateT[IO, S, A]

    private final val actor = Actor[A]({
      // Close over a local cell containing our state.
      var state = initialState
      (a: A) => state = act(a).exec(state).unsafePerformIO
    }, _.printStackTrace)

    final def !(a: A): Unit = actor ! a

    def queue(a:A) = IO { actor ! a }.liftIO[IOState]
    
    def nop = IO().liftIO[IOState]
    
    def act(a: A): IOState[Unit]

  }

  val adder = new StatefulActor[Int, String](0) {
    def act(s: String): IOState[Unit] =
      for {
        _ <- modify[Int](_ + s.length).lift[IO]
        t <- get.lift[IO]
        _ <- putStrLn("added " + s + "; total length is " + t).liftIO[IOState]
        _ <- if (t < 20) act("foo") else nop
      } yield ()
  }

  adder ! "apple"
  adder ! "orange"
  adder ! "pear"

  Thread.sleep(10000)

//	added apple; total length is 5
//	added foo; total length is 8
//	added foo; total length is 11
//	added foo; total length is 14
//	added foo; total length is 17
//	added foo; total length is 20
//	added orange; total length is 26
//	added pear; total length is 30

}