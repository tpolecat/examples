package eg

import scalaz.effect.IO
import scalaz.effect.IO._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scalaz.Bind
import scalaz.syntax.monad._

// Wrap a future such that you can only see it in IO
final class IOFuture[+A] private (f: Future[A])(implicit ec: ExecutionContext) {

  def map[B](g: A => B): IOFuture[B] =
    new IOFuture(f.map(g))

  def flatMap[B](g: A => IOFuture[B]): IOFuture[B] =
    new IOFuture(Future(Await.result(f.map(g), Duration.Inf).await.unsafePerformIO))

  def await: IO[A] =
    IO(Await.result(f, Duration.Inf))

  def onSuccess(a: A => IO[Unit]): IO[Unit] =
    IO(f.onSuccess { case r => a(r).unsafePerformIO })

  def isComplete: IO[Boolean] =
    IO(f.isCompleted)

}

object IOFuture {
  
  def newIOFuture[A](a: IO[A])(implicit ec: ExecutionContext): IO[IOFuture[A]] =
    IO(new IOFuture(Future(a.unsafePerformIO)))

  implicit class pimpIO[A](a: IO[A])(implicit ec: ExecutionContext) {
    def future: IO[IOFuture[A]] = newIOFuture(a)
  }

  implicit object BindIOFuture extends Bind[IOFuture] {
    def bind[A, B](fa: IOFuture[A])(f: A => IOFuture[B]): IOFuture[B] = fa flatMap f
    def map[A, B](fa: IOFuture[A])(f: A => B): IOFuture[B] = fa map f
  }

}

// An example
object IOFutureTest extends App {

  import IOFuture._
  import ExecutionContext.Implicits.global

  val x = for {

    __ <- putStrLn("Creating futures")
    c1 <- getChar.future
    c2 <- getChar.future

    // All of the onComplete calls can run in any order
    _ <- c1.onSuccess(c => putStrLn("(1) c1 got " + c))
    _ <- c1.onSuccess(c => putStrLn("(2) c1 got " + c))
    _ <- c2.onSuccess(c => putStrLn("c2 got " + c))

    // IOFuture composes
    pair = for {
      a <- c1
      b <- c2
    } yield (a, b)

    _ <- putStrLn("waiting")
    _ <- c1.isComplete.map(_.toString) >>= putStrLn // false
    ps <- pair.await // blocks
    _ <- c1.isComplete.map(_.toString) >>= putStrLn // true
    _ <- putStrLn("got " + ps)

  } yield ps

  println(x.unsafePerformIO)

}

