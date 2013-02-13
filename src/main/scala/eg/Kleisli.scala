package examples

import language.higherKinds

import scalaz._
import scalaz.Kleisli._
import scalaz.Id._
import scalaz.std.AllInstances._

object KleisliTest extends App {

  // Kleisli is aliased as ReaderT

  def foo[M[+_]: Monad]: Kleisli[M, String, Int] = for {
    s <- ask[M, String]
  } yield s.length

  println(foo[Id].run("foo"))
  println(foo[Option].run("foo"))
  println(foo[List].run("foo"))

  
  implicit class Foo[A](as:Seq[A]) {
    def randomIndex = util.Random.nextInt(as.length)
    def randomElement:Option[A] = if (as.isEmpty) None else Some(as(randomIndex))
  }
  
  println(new Foo(List(1,2,3,4,5)).randomElement)
 
  
  
}