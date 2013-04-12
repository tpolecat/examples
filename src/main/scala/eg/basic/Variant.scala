package eg.basic

import scalaz._
import Scalaz._

object Variants {

  import Errors._

  sealed trait Type {
    def name: String
    override def toString = name
  }

  case object TInteger extends Type { def name = "Integer" }
  case object TString extends Type { def name = "String" }
  case object TDouble extends Type { def name = "Real" }
  case object TNumeric extends Type { def name = "Numeric" }

  sealed trait Variant {
	
    type VOp = Variant.VOp
	type Answer = Variant.Answer
    
    def + : VOp
    def - : VOp
    def * : VOp
    def / : VOp

    def =? : VOp
    def <> : VOp
    def >= : VOp
    def <= : VOp
    def > : VOp
    def < : VOp

    def & : VOp
    def | : VOp

    def unary_~ : Answer
    def unary_- : Answer

    def vtype: Type

  }

  object Variant {
    type Answer = Validation[Error, Variant]
    type VOp = Variant => Answer

    def apply(d: Double): Variant = VNumber(if (d == d.toInt) -\/(d.toInt) else \/-(d))
    def apply(i: Int): Variant = VNumber(-\/(i))
    def apply(s: String) = VString(s)
  }

  val T: Variant = VNumber(-\/(-1))
  val F: Variant = VNumber(-\/(0))

  /** Variant numeric type whose representation is Int if possible, or Double if needed. */
  case class VNumber(e: Int \/ Double) extends Variant {

    def vtype = e.fold(_ => TInteger, _ => TDouble)

    private implicit class PimpE(e: Int \/ Double) {
      def toDouble = e.fold(_.toDouble, identity)
    }

    // Some aliases
    private type C[A] = (A, A) => A
    private type P[A] = (A, A) => Boolean
    private type B[A] = (A, A) => Answer
    private type U[A] = A => Answer

    // For operations on VNumber we have two cases. We can either do integer operations or double operations.
    private def op(i: B[Int], d: B[Double], widen: P[Int] = (_, _) => false): VOp = {
      case VString(_) => TypeMismatch(TNumeric, TString).fail
      case VNumber(b) => (e, b) match {
        case (-\/(a), -\/(b)) if !widen(a, b) => i(a, b)
        case _                                => d(e.toDouble, b.toDouble)
      }
    }

    // For operations on VNumber we have two cases. We can either do integer operations or double operations.
    private def uop(i: U[Int], d: U[Double]): Answer = e.fold(i, d)

    // Arithmetic
    private def aop(i: C[Int], d: C[Double], widen: P[Int] = (_, _) => false): VOp =
      op((a, b) => VNumber(-\/(i(a, b))).success, (a, b) => VNumber(d(a, b)).success, widen)

    // Comparison 
    private def cop(i: P[Int], d: P[Double]): VOp =
      op((a, b) => (i(a, b) ? T | F).success, (a, b) => (d(a, b) ? T | F).success)

    // Bitwise (defined for Int only)
    private def bop(i: C[Int]): VOp =
      op((a, b) => VNumber(-\/(i(a, b))).success, (_, _) => TypeMismatch(TInteger, TDouble).fail)

    def + = aop(_ + _, _ + _)
    def - = aop(_ - _, _ - _)
    def * = aop(_ * _, _ * _)
    def / = aop(_ / _, _ / _, _ % _ != 0)

    def =? = cop(_ == _, _ == _)
    def <> = cop(_ != _, _ != _)
    def >= = cop(_ >= _, _ >= _)
    def <= = cop(_ <= _, _ <= _)
    def > = cop(_ > _, _ > _)
    def < = cop(_ < _, _ < _)

    def & = bop(_ & _)
    def | = bop(_ | _)

    def unary_~ = uop(i => VNumber(-\/(~i)).success, _ => TypeMismatch(TInteger, TDouble).fail)
    def unary_- = uop(i => VNumber(-\/(-i)).success, d => VNumber(\/-(-d)).success)

  }

  object VNumber {
    def apply(d: Double): VNumber = VNumber(if (d == d.toInt) -\/(d.toInt) else \/-(d))
    def apply(i: Int): VNumber = VNumber(-\/(i))
  }

  case class VString(s: String) extends Variant {
    def vtype = TString

    private val fail = TypeMismatch(TString, TNumeric).fail
    private val numericOnly: VOp = _ => fail

    private def op(f: (String, String) => Answer): VOp = {
      case VNumber(_) => TypeMismatch(TString, TNumeric).fail
      case VString(t) => f(s, t)
    }

    private def cop(f: (String, String) => Boolean): VOp =
      op((a, b) => (f(a, b) ? T | F).success)

    def + : VOp = op((a, b) => VString(a + b).success)
    def - : VOp = numericOnly
    def * : VOp = numericOnly
    def / : VOp = numericOnly

    def =? : VOp = cop(_ == _)
    def <> : VOp = cop(_ != _)
    def >= : VOp = cop(_ >= _)
    def <= : VOp = cop(_ <= _)

    def > : VOp = cop(_ > _)
    def < : VOp = cop(_ < _)

    def & : VOp = numericOnly
    def | : VOp = numericOnly

    def unary_~ = fail
    def unary_- = fail

  }

}