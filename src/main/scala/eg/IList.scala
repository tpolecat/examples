package eg

import language._
import annotation.tailrec
import scalaz._
import Scalaz._

sealed trait IList[A] {

  // Fundamentals (to get a tail-recursive foldRight)
  def foldLeft[B](b: B)(f: (B, A) => B): B = IList.foldL(this)(b)(f)
  def foldRight[B](b: B)(f: (A, B) => B): B = reverse.foldLeft(b)((b, a) => f(a, b))
  def reverse: IList[A] = foldLeft(INil[A]())((as, a) => a :: as)

  // List-specific things
  def headOption: Option[A]
  def tailOption: Option[IList[A]]

  // Basic syntax
  def ::(a: A) = ICons(a, this) 
  def ++(as: IList[A]): IList[A] = foldRight(as)(_ :: _)

  // Ops for `for` comprehensions
  def foreach(f: A => Unit): Unit = foldLeft(()) { (_, a) => f(a); () }
  def map[B](f: A => B): IList[B] = foldRight(INil[B])(f(_) :: _)
  def flatMap[B](f: A => IList[B]): IList[B] = foldRight(INil[B])(f(_) ++ _)
  def filter(f: A => Boolean): IList[A] = foldRight(INil[A])((a, as) => f(a) ? (a :: as) | as)

  // Utilities
  def toList: List[A] = foldRight(Nil : List[A])(_ :: _)

}

class INil[A] private extends IList[A] {
  def headOption: Option[A] = None
  def tailOption: Option[IList[A]] = None
}

object INil {
  def apply[A](): IList[A] = new INil[A]
  def unapply[A](as: INil[A]): Some[Unit] = Some(())
}

class ICons[A] private (val head: A, val tail: IList[A]) extends IList[A] {
  def headOption: Option[A] = Some(head)
  def tailOption: Option[IList[A]] = Some(tail)
}

object ICons {
  def apply[A](hd: A, tl: IList[A]): IList[A] = new ICons(hd, tl)
  def unapply[A](as: ICons[A]): Some[(A, IList[A])] = Some((as.head, as.tail))
}

object IList {

  def apply[A](as: A*): IList[A] = as.foldRight(INil[A]())(ICons(_, _))
  def fromList[A](as: List[A]): IList[A] = apply(as: _*)

  @tailrec final def foldL[A,B](as: IList[A])(b: B)(f: (B, A) => B): B =
    as match {
      case INil() => b
      case ICons(a, as) => foldL[A,B](as)(f(b, a))(f)
    }

  implicit val instances =
    new Traverse[IList] with MonadPlus[IList] with Each[IList] with Index[IList] with Length[IList] with Zip[IList] with IsEmpty[IList] { // with Unzip[IList] with Cobind[List] {

      // Traverse
      def traverseImpl[F[_], A, B](fa: IList[A])(f: A => F[B])(implicit F: Applicative[F]): F[IList[B]] =
        fa.foldRight(F.point(INil[B]))((a, fbs) => F.apply2(f(a), fbs)(_ :: _))

      // MonadPlus, Each, Length, IsEmpty
      override def map[A, B](fa: IList[A])(f: A => B): IList[B] = fa map f
      def point[A](a: => A): IList[A] = IList(a)
      def bind[A, B](fa: IList[A])(f: A => IList[B]): IList[B] = fa flatMap f
      def plus[A](a: IList[A],b: => IList[A]): IList[A] = a ++ b
      def empty[A]: IList[A] = INil[A]()
      def each[A](fa: IList[A])(f: A => Unit): Unit = fa foreach f
      def length[A](fa: IList[A]): Int = fa.foldLeft(0)((n, _) => n + 1)
      def isEmpty[A](fa: IList[A]): Boolean = fa.headOption.isEmpty

      // Index
      @tailrec def index[A](fa: IList[A], i: Int): Option[A] =
        fa match {
          case ICons(a, as) => if (i == 0) Some(a) else index[A](as, i - 1)
          case INil() => None
        }

      // Zip
      def zip[A, B](a: => IList[A], b: => IList[B]): IList[(A, B)] = {
        @tailrec def zaccum(a: => IList[A], b: => IList[B], accum: IList[(A,B)]): IList[(A, B)] =
          (a, b) match {
            case (ICons(a, as), ICons(b, bs)) => zaccum(as, bs, (a, b) :: accum)
            case _ => accum
          }
        zaccum(a, b, INil()).reverse
      }

    }

  implicit def equal[A](implicit A: Equal[A]): Equal[IList[A]] =
    new Equal[IList[A]] {
      @tailrec def equal(a: IList[A], b: IList[A]): Boolean =
        (a, b) match {
          case (INil(), INil()) => true
          case (ICons(a, as), ICons(b, bs)) if A.equal(a, b) => equal(as, bs)
          case _ => false
        }
    }

  implicit def monoid[A]: Monoid[IList[A]] =
    new Monoid[IList[A]] {
      def append(f1: IList[A], f2: => IList[A]) = f1 ++ f2
      def zero: IList[A] = INil()
    }

  implicit def show[A](implicit A: Show[A]): Show[IList[A]] = 
    new Show[IList[A]] {
      override def show(as: IList[A]) = {
        @tailrec def commaSep(rest: IList[A], acc: Cord): Cord =
          rest match {
            case INil() => acc
            case ICons(x, xs) => commaSep(xs, (acc :+ ",") ++ A.show(x))
          }
        "[" +: (as match {
          case INil() => Cord()
          case ICons(x, xs) => commaSep(xs, A.show(x))
        }) :+ "]"
      }
    }

}

