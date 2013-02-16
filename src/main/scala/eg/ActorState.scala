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
      (a: A) => state = act(a)(state).unsafePerformIO._1
    }, _.printStackTrace)

    final def !(a: A): Unit = actor ! a

    def act(a: A): IOState[Unit]

  }

  val adder = new StatefulActor[Int, String](0) {
    def act(s: String): IOState[Unit] =
      for {
        _ <- modify[Int](_ + s.length).lift[IO]
        t <- get.lift[IO]
        _ <- putStrLn("added " + s + "; total length is " + t).liftIO[IOState]
      } yield ()
  }

  adder ! "apple"
  adder ! "orange"
  adder ! "pear"

  Thread.sleep(10000)

  // added apple; total length is 5
  // added orange; total length is 11
  // added pear; total length is 15

}