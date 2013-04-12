package eg.basic

import Variants._

object ASTExpr {

  // Unary Oprators
  sealed abstract class UnOp(val vop: Variant => Variant.Answer)
  case object NEG extends UnOp(-_)
  case object NOT extends UnOp(~_)

  // Binary Operators
  sealed abstract class BinOp(val vop: Variant => Variant.VOp)
  case object EQ extends BinOp(_.=?)
  case object NE extends BinOp(_.<>)
  case object LT extends BinOp(_.<)
  case object LE extends BinOp(_.<=)
  case object GT extends BinOp(_.>)
  case object GE extends BinOp(_.>=)
  case object Plus extends BinOp(_.+)
  case object Minus extends BinOp(_.-)
  case object Times extends BinOp(_.*)
  case object Div extends BinOp(_./)

  // Expressions.
  sealed trait Expr
  case class Var(s: Symbol) extends Expr
  case class Lit(v: Variant) extends Expr
  case class Bin(a: Expr, op: BinOp, b: Expr) extends Expr
  case class Un(op: UnOp, a: Expr) extends Expr

  // Library Functions
  case class Int0(e: Expr) extends Expr

}