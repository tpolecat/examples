package eg

object Unfold extends App {

  // The general unfold
  def unfold[A](a: A)(f: A => Option[A]): Stream[A] =
    f(a).map(a => a #:: unfold(a)(f)).getOrElse(Stream.empty)

  def hailstone(i: Int) = i #:: unfold(i) { n =>
    if (n == 1) None else Some(if (n % 2 == 0) n / 2 else n * 3 + 1)
  }

  println(hailstone(27).toList)
  val (n, len) = (1 to 100000).map(n => (n, hailstone(n).length)).maxBy(_._2)
  println("value=" + n + "  len=" + len)

}