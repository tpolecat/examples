package eg

import scalaz._
import scalaz.State._
import scalaz.StateT
import scalaz.std.option._
import scalaz.effect.IO
import scalaz.effect.IO._
import scalaz.effect.stateTEffect.StateTMonadIO
import scalaz.OptionT
import scalaz.effect.LiftIO
import scalaz.effect.MonadIO

object StateTExample extends App {

//  // To avoid the type lambda ({type λ[a] = StateT[IO,String,a]})#λ
  //  // in the call to liftIO below, we will just make an alias.
  //  type StringIO[+A] = StateT[IO, String, A]
  //
  //  // The tricky part is the lifting. 
  //  val action: StringIO[Int] =
  //    for {
  //      s <- get[String].lift[IO]
  //      _ <- put("foo").lift[IO]
  //      _ <- putStrLn("initial state was " + s).liftIO[StringIO]
  //    } yield s.length
  //
  //  val a: IO[(String, Int)] = action.run("hello")
  //  val (s, i) = a.unsafePerformIO
  //
  //  println("result was %d, final state was %s".format(i, s))

}

object StateTExample2 extends App {

  type Running = String

  type M[A] = EitherT[IO, Error, A]

  type Op[A] = StateT[({ type λ[+α] = EitherT[IO, Error, α] })#λ, Running, A]

  implicit def liftio = new MonadIO[Op] {
    def point[A](a: => A): Op[A] = unit(a)
    def bind[A, B](fa: Op[A])(f: A => Op[B]): Op[B] = fa.flatMap(f)
    def liftIO[A](ioa: IO[A]): Op[A] = StateT[M, Running, A] { r =>
      EitherT(ioa.map(a => \/-((r, a))))
    }
  }

  /** Operation to return the current `Running` state. */
  def get: Op[Running] = State.get.lift[M]

  /** Operation to return the current `Running` state, mapped to some type `A`. */
  def gets[A](f: Running => A): Op[A] = State.gets(f).lift[M]

  /** Operation to modify the current `Running` state. */
  def modify(f: Running => Running): Op[Unit] = State.modify(f).lift[M]

  /** Operation to replace the current `Running` state. */
  def put(s: Running): Op[Unit] = State.put(s).lift[M]

  /** Operation that returns the passed value. */
  def unit[A](a: A): Op[A] = State.state(a).lift[M]

  val action =
    for {
      a <- gets(_.length)
      b <- unit(5)
      _ <- putStr("name? ").liftIO[Op]
      s <- readLn.liftIO[Op]
      _ <- modify(_ + " " + s)
      c <- get
    } yield (a, b, c)

  val a = action("foo").run.unsafePerformIO

  println(a)

}
