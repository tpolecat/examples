package eg

import scalaz.State._
import scalaz.StateT
import scalaz.effect.IO
import scalaz.effect.IO._
import scalaz.effect.stateTEffect.StateTMonadIO

object StateTExample extends App {

  // To avoid the type lambda ({type λ[+a] = StateT[IO,String,a]})#λ
  // in the call to liftIO below, we will just make an alias.
  type StringIO[+A] = StateT[IO, String, A]
    
  // The tricky part is the lifting. 
  val action: StringIO[Int] =
    for {
      s <- get[String].lift[IO]
      _ <- put("foo").lift[IO]
      _ <- putStrLn("initial state was " + s).liftIO[StringIO]
    } yield s.length

  val a: IO[(String, Int)] = action.run("hello")
  val (s, i) = a.unsafePerformIO

  println("result was %d, final state was %s".format(i, s))

}

