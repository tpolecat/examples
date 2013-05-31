package eg

// The demonstrates the isomorphism between list and fold
object FoldStuff extends App {

  trait MyList[A] { outer =>

    def fold[B](k: Option[(A, B)] => B): B

    def ::(a: A) = new MyList[A] {
      def fold[B](k: Option[(A, B)] => B) =
        k(Some(a, outer fold k))
    }

    def length = fold[Int] {
      case None => 0
      case Some((_, n)) => n + 1
    }

    def map[B](f: A => B) = fold[MyList[B]] {
      case None => MyList.nil
      case Some((a, as)) => f(a) :: as
    }

    def ++(other: MyList[A]): MyList[A] = fold[MyList[A]] {
      case None => other
      case Some((a, accum)) => a :: accum
    }

    def flatMap[B](f: A => MyList[B]) = fold[MyList[B]] {
      case None => MyList.nil
      case Some((a, as)) => f(a) ++ as
    }

    override def toString = "MyList(" ++ fold[String] {
      case None => ")"
      case Some((a, ")")) => a + ")"
      case Some((a, s)) => a + "," + s
    }

  }

  object MyList {

    def nil[A]: MyList[A] = new MyList[A] {
      def fold[B](k: Option[(A, B)] => B): B = k(None)
    }

    def unfold[A, B](b: B)(f: B => Option[(A, B)]): MyList[A] = f(b) match {
      case None => nil[A]
      case Some((a, b)) => a :: unfold(b)(f)
    }

    // when A =:= B
    def unfold0[A](a: A)(f: A => Option[A]) =
      unfold(a)(b => f(b).map(b => (b, b)))

    def to[A](l: List[A]): MyList[A] =
      new MyList[A] {
        def fold[B](k: Option[(A, B)] => B): B =
          l match {
            case Nil => k(None)
            case h :: t => k(Some(h, to(t) fold k))
          }
      }

    def fr[A](l: MyList[A]): List[A] =
      l fold ((_: Option[(A, List[A])]) match {
        case None => Nil
        case Some((a, b)) => a :: b
      })

  }

  // Simple tests
  import MyList._
  val ns = 'x' :: 'y' :: nil
  println(nil)
  println("foo" :: nil)
  println(ns)
  println(ns.length)
  println(ns.map(_.toInt))
  println(ns ++ ns)

  // Flatmap
  println(for {
    c <- to("abc".toList)
    n <- to(List(1, 2, 3))
  } yield (c, n))

  // Hailstone
  def hailstone(i: Int) = i :: MyList.unfold0(i) { n =>
    if (n == 1) None else Some(if (n % 2 == 0) n / 2 else n * 3 + 1)
  }
  println(fr(hailstone(27)))
  val (n, len) = (1 to 100000).map(n => (n, hailstone(n).length)).maxBy(_._2)
  println("value=" + n + "  len=" + len)

}