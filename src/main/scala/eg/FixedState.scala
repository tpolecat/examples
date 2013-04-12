package eg

/**
 * Unboxed State[S,A] with constrained S (which helps with type inference).
 * See Pascal for example.
 */
trait FixedState[S] {

  type State[+A] = S => (S, A)

  implicit class StateOps[+A](run: State[A]) {
    def map[B](f: A => B): State[B] = { s =>
      val (s0, a) = run(s)
      (s0, f(a))
    }
    def flatMap[B](f: A => State[B]): State[B] = { s =>
      val (s0, a) = run(s)
      f(a)(s0)
    }
  }

  implicit class AnyOpts[+A](a: A) {
    def unit[S]: State[A] = (_, a)
  }

  def mod(f: S => S): State[Unit] = s => (f(s), ())
  def get[A](f: S => A): State[A] = s => (s, f(s))

}

