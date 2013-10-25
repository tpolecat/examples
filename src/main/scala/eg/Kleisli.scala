package eg

import language.higherKinds

import scalaz._
import Scalaz._
import scalaz.Kleisli._
// import scalaz.Id._
// import scalaz.std.AllInstances._
// 
object KleisliTest extends App {

  // Kleisli is aliased as ReaderT
  def foo[M[+_]: Monad]: Kleisli[M, String, Int] = for {
    s <- ask[M, String]
    t <- ask[M, String]
  } yield (s ++ t).length

  println(foo[Id].run("foo"))
  println(foo[Option].run("foo"))
  println(foo[List].run("foo"))
  
}
