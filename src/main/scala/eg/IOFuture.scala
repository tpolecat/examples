package eg

import scalaz.effect.IO
import scalaz.effect.IO._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration

final class IOFuture[+A] private (f: => Future[A])(implicit ec: ExecutionContext) {

  def get: IO[A] = IO(Await.result(f, Duration.Inf))

  def onComplete(a: A => IO[Unit]): IO[Unit] = IO(f.onSuccess { case r => a(r).unsafePerformIO })

  def map[B](g: A => B): IOFuture[B] = new IOFuture(f.map(g))

  def flatMap[B](g: A => IOFuture[B]): IOFuture[B] =
    new IOFuture(Future(Await.result(f.map(g), Duration.Inf).get.unsafePerformIO))

}

object IOFuture {

  def newIOFuture[A](a: IO[A])(implicit ec: ExecutionContext): IO[IOFuture[A]] =
    IO(new IOFuture(Future(a.unsafePerformIO)))

}

object IOFutureTest extends App {

  import IOFuture._
  import ExecutionContext.Implicits.global

  val x = for {
    
    __ <- putStrLn("Creating futures")
    c1 <- newIOFuture(getChar)
    c2 <- newIOFuture(getChar)

    pair = for {
      a <- c1
      b <- c2
    } yield (a, b)

    __ <- putStrLn("waiting")
    ps <- pair.get
    _  <- putStrLn("got " + ps)
    
  } yield ps
  
  println(x.unsafePerformIO)

}