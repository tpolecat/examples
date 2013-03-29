package eg

import scalaz.concurrent.MVar.newEmptyMVar
import scalaz.concurrent.Strategy
import scalaz.effect.IO
import scalaz.effect.IO.putStrLn

object MVarIssue extends App {

  def forkIO(f: => IO[Unit])(implicit s: Strategy): IO[Unit] =
    IO { s(f.unsafePerformIO); () }

  val bug: IO[Unit] =
    for {
      in <- newEmptyMVar[String]
      _ <- forkIO {
        for {
          a <- in.take
          b <- in.take
          c <- in.take
          _ <- putStrLn((a, b, c).toString)
        } yield ()
      }
      _ <- in.put("foo")
      _ <- in.put("bar")
      _ <- in.put("baz")
    } yield ()

  (1 to 10) foreach { n =>
    bug.unsafePerformIO
  }

	// Results are nondeterministic
	//	(foo,foo,baz)
	//	(foo,foo,foo)
	//	(foo,foo,foo)
	//	(foo,foo,foo)
	//	(foo,foo,foo)
	//	(foo,foo,foo)
	//	(foo,foo,foo)
	//	(foo,foo,foo)
	//	(foo,bar,baz)
	//	(foo,bar,baz)

}
