package eg

import scalaz._
import scalaz.effect._
import scalaz.effect.Effect._
import scalaz.effect.IO._
import scalaz.effect.ST._
import scalaz.syntax.monad._

object ST extends App {

  // ST actions must be parameterized by their thread "S", which always remains free
  def a[S]: ST[S, Int] = returnST(66).map(_ + 1)

  // In order to run an action, you need to construct a Forall, which is awkward.
  // Note that it fixes the return type.
  val forall = new Forall[({ type λ[σ] = ST[σ, Int] })#λ] { def apply[S] = a }

  // We can abstract out the lambda type if we want to
  type ForallST[A] = Forall[({ type λ[σ] = ST[σ, A] })#λ]
  val forall2 = new ForallST[Int] { def apply[S] = a }

  // They should be equivalent (N.B. might crash presentation compiler but does work)
  implicitly[Forall[({ type λ[σ] = ST[σ, Int] })#λ] =:= ForallST[Int]]

  // this is pure
  val x0 = runST(forall)

  // this may not be
  val x1 = a[IvoryTower].unsafePerformIO

  // this is in IO (no forall required)
  val z = for {
    n <- IO(1)
    v <- newVar[IvoryTower](20)
    _ <- v |= 4
    _ <- v.mod(_ * 2)
    x <- a // calling a pure ST action from IO
    y <- v.read
    _ <- putStrLn("x was " + x)
    a <- newArr[IvoryTower, String](10, "x")
    b <- a.write(2, "bar")
    _ <- b.write(3, "baz")
    a0 <- a.freeze
    b0 <- b.freeze
  } yield (n, x, y, a0.toList, b0.toList)

  println(z.unsafePerformIO)

}

