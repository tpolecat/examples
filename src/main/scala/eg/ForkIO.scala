package eg

import scalaz.concurrent.MVar.newEmptyMVar
import scalaz.concurrent.Strategy
import scalaz.effect.IO
import scalaz.effect.IO._
import scalaz.concurrent.MVar
import scalaz.concurrent.Promise
import scalaz.syntax.monad._
import java.util.logging.Logger
import java.util.logging.Level.WARNING

class Async[A] private (private val mvar: MVar[A]) {
  def await: IO[A] = mvar.read
}

object Async {

  private val Log = Logger.getLogger(getClass.getName)

  def future[A](action: IO[A])(implicit s: Strategy): IO[Async[A]] =
    newEmptyMVar[A] map { m =>
      s(action.flatMap(m.put(_)).except(e => IO {
        Log.log(WARNING, "Unhandled exception in acync action.", e)
      }).unsafePerformIO)
      new Async(m)
    }

  
//  def async[A,B](producer:IO[A], consumer:A => IO[B]):IO[MVar[B]] = ???
  
}

object AsyncTest extends App {

  import Async._

  val c = for {
    f <- future(getChar)
    _ <- putStrLn("waiting..")
    c <- f.await
    _ <- putStrLn("got a char..")
  } yield c

  println(c.unsafePerformIO)

}


//
//  val bug: IO[Unit] =
//    for {
//      in <- newEmptyMVar[String]
//      _ <- forkIO {
//        for {
//          a <- in.take
//          b <- in.take
//          c <- in.take
//          _ <- putStrLn((a, b, c).toString)
//        } yield ()
//      }
//      _ <- in.put("foo")
//      _ <- in.put("bar")
//      _ <- in.put("baz")
//    } yield ()
//
//  (1 to 10) foreach { n =>
//    bug.unsafePerformIO
//  }
//
//  // Results are nondeterministic
//  //	(foo,foo,baz)
//  //	(foo,foo,foo)
//  //	(foo,foo,foo)
//  //	(foo,foo,foo)
//  //	(foo,foo,foo)
//  //	(foo,foo,foo)
//  //	(foo,foo,foo)
//  //	(foo,foo,foo)
//  //	(foo,bar,baz)
//  //	(foo,bar,baz)
//
//}
