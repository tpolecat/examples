package eg

import language.higherKinds

import scalaz._
import scalaz.effect._
import scalaz.syntax.monad._
import scalaz.syntax.plus._
import scalaz.effect.IO._
import scalaz.Free._

object IOTest extends App {

  // An effect that prints the current amount of free memory.
  val printHeap: IO[Unit] =
    for {
      h <- IO { Runtime.getRuntime.freeMemory }
      _ <- putStrLn("Heap: " + h)
    } yield ()

  // This kind of combinator is the only apparent way to have a stack/heap-safe 
  // loop. The recursion must be inside the final flatMap.
  def loop[A](a: => IO[A]): IO[A] = a.flatMap(_ => loop(a))

  // This is the general unfoldM, but it's not what we want here
  def unfold[M[_]: MonadPlus, A, B](f: B => Option[(M[A], B)])(b: B): M[A] =
    f(b) match {
      case Some((a, newB)) => a <+> unfold(f)(newB)
      case None => MonadPlus[M].empty
    }

  // This one is better
  def unfoldM[M[_]: Monad, A](a: A)(f: A => M[Option[A]]): M[A] =
    f(a).flatMap(_.map(unfoldM(_)(f)).getOrElse(a.pure[M]))

  // A test
  def count(to: Int)(n: Int): IO[Option[Int]] =
    for {
      _ <- putStr(n + ": ")
      _ <- printHeap
    } yield if (n == to) None else Some(n + 1)

  // This will consume neither stack nor heap
  println("Result was " + unfoldM(1)(count(500000)).unsafePerformIO)

}

