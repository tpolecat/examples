package eg.basic

import scalaz._
import Scalaz._
import Errors._

object Variants {

  /** Types are used in error reporting and to classify variables. */
  sealed trait Type {
    def name: String
    override def toString = name
  }

  /** Variants have specific types. */
  sealed trait VariantType extends Type

  /** General numeric type. Only used in error reporting. */
  case object TNumeric extends Type { def name = "Numeric" }

  case object TInteger extends VariantType { def name = "Integer" }
  case object TString extends VariantType { def name = "String" }
  case object TDouble extends VariantType { def name = "Real" }

  /**
   * A value type that's either numeric or a `String`, and if it's numeric it's either an `Int` or a `Double`. This
   * type is isomorphic to `(Int \/ Double) \/ String`, but it turns out to be nice to have distinct constructors and
   * a place to hang the operations.
   */
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

    def vtype: VariantType

  }

  object Variant {

    type Answer = Validation[Error, Variant]
    type VOp = Variant => Answer

    /**
     * Construct a `Variant` from the passed `Double`, which will be demoted to `Int` if this can be done without loss
     * of precision. Numerics will be stored as `Int` whenever possible; the result of an operation on two `Double`
     * values might be an `Int`.
     */
    def apply(d: Double): Variant = VNumber(if (d == d.toInt) -\/(d.toInt) else \/-(d))

    /** Construct a `Variant` from the passed `Int`. */
    def apply(i: Int): Variant = VNumber(-\/(i))

    /** Construct a `Variant` from the passed `String`. */
    def apply(s: String) = VString(s)

  }

  /** Boolean value true is the numeric value `-1` */
  val T: Variant = VNumber(-\/(-1))

  /** Boolean value false is the numeric value `0` */
  val F: Variant = VNumber(-\/(0))

  /** `Variant` numeric type whose representation is `Int` if possible, or `Double` if needed. */
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
      op((a, b) => VNumber(-\/(i(a, b))).success, (a, b) => Variant(d(a, b)).success, widen)

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

  /** `Variant` string type. */
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