package eg

object Unfold extends App {

  // Genera form
  def unfold[A, B](a: A)(f: A => Option[(A, B)]): Stream[B] =
    f(a).map { case (a, b) => b #:: unfold(a)(f) }.getOrElse(Stream.empty)

  // A less general form
  def unfold0[A](a: A)(f: A => Option[A]) =
    unfold(a)(a => f(a).map(a => (a, a)))

  def hailstone(i: Int) = i #:: unfold0(i) {
    case 1 => None
    case n => Some(if (n % 2 == 0) n / 2 else n * 3 + 1)
  }

  println(hailstone(27).toList)
  val (n, len) = (1 to 100000).map(n => (n, hailstone(n).length)).maxBy(_._2)
  println("value=" + n + "  len=" + len)

}