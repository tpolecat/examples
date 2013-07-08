package eg

import scalaz._
import Scalaz._
import language._

// from https://www.fpcomplete.com/user/bartosz/understanding-algebras
object Alg extends App {

  // Expressions (non-recursive)
  sealed trait ExprF[A]
  case class Const[A](n: Int) extends ExprF[A]
  case class Add[A](a: A, b: A) extends ExprF[A]
  case class Mul[A](a: A, b: A) extends ExprF[A]

  // Fixpoint for type constructor F
  case class Fix[F[_]](unFix: F[Fix[F]])

  // Expressions (recursive)
  type Expr = Fix[ExprF]

  // An example of this type
  val testExpr: Expr = 
    Fix(Add(Fix(Const(2)), 
            Fix(Mul(Fix(Const(3)), 
                    Fix(Const(4))))))

  // ExprF has a functor
  implicit object ExprFFunctor extends Functor[ExprF] {
    def map[A, B](fa: ExprF[A])(eval: A => B): ExprF[B] = fa match {
      case Const(i)         => Const(i)
      case Add(left, right) => Add(eval(left), eval(right))
      case Mul(left, right) => Mul(eval(left), eval(right))
    }
  }

  // An F-algebra
  type Algebra[F[_], A] = F[A] => A

  // F-algebra for ExprF with carrier type Int and function alg
  type SimpleA = Algebra[ExprF, Int]
  val alg : SimpleA = {
    case Const(i)  => i
    case Add(x, y) => x + y
    case Mul(x, y) => x * y
  }

  // Our initial algebra is just Fix
  type ExprInitAlg = Algebra[ExprF, Fix[ExprF]]
  val ex_init_alg: ExprInitAlg = Fix.apply _

  // Catamorphism
  def cata[F[_] : Functor, A](alg: F[A] => A)(x: Fix[F]): A = 
    alg(x.unFix.map(cata(alg)))

  // Our evaluator
  val eval: Expr => Int = 
    cata(alg)

  // Prints 14
  println(eval(testExpr))

  // Can't figure out how to do the foldr example with the partially-applied
  // type ctor. Would be interested in seeing a solution.

}

