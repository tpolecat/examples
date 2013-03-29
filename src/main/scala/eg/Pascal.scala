package eg

/**
 * Pascal's triangle with memoization via the State monad. This is not nearly as fast
 * as memo-passing via an explicit parameter, but the code path is less cluttered.
 * Note that this doesn't fix stack issues.
 */
object Pascal extends App {

  object MemoMonad extends FixedState[Map[(BigInt, BigInt), BigInt]]
  import MemoMonad._

  def pascal(c: Int, r: Int): BigInt = {

    def pascal1(c: BigInt, r: BigInt): State[BigInt] =
      get(_.lift((c, r))) flatMap {
        case Some(a) => a.unit
        case None =>
          if (c < 0 || c > r) BigInt(0).unit
          else if (r == 0) BigInt(1).unit
          else for {
            a <- pascal1(c, (r - 1))
            b <- pascal1((c - 1), (r - 1))
            _ <- mod(_ + ((c, r) -> (a + b)))
          } yield (a + b)
      }

    pascal1(c, r)(Map())._2

  }

  def time[A](a: => A): A = {
    val s = System.currentTimeMillis
    try a finally println(System.currentTimeMillis - s)
  }

  time(println(pascal(100, 200)))

}