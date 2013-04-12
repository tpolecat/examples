package eg.basic.interp

import scalaz._
import scalaz.syntax.monad._
import scalaz.syntax.std.boolean._
import eg.basic._
import eg.basic.ASTExpr._
import eg.basic.Errors._
import eg.basic.Variants._

/** An expression evaluator for our low-level machine. */
trait ExprMachine { this: StateMachine with LLMachine =>

  /** Evaluate the given expression and return its value, or halt with an error. */
  def eval(e: Expr): Op[Variant] = e match {

    case Lit(v)    => unit(v)
    case Var(s)    => lookup(s)
    case Un(op, e) => eval(e).map(op.vop) >>= (_.fold(trap, unit))

    case Bin(e1, op, e2) => for {
      a <- eval(e1)
      b <- eval(e2)
      c <- op.vop(a)(b).fold(trap, unit)
    } yield c

    case Int0(v) => eval(v) >>= {
      case v @ VNumber(-\/(_)) => unit(v)
      case VNumber(\/-(d))     => unit(VNumber(-\/(d.toInt)))
      case _                   => trap(TypeMismatch(TNumeric, TString))
    }

  }

  /** Evaluate the given expression and return its value iff it is an integer, or halt with an error. */
  def evalInt(e: Expr): Op[Int] = eval(e) >>= {
    case VNumber(-\/(n)) => unit(n)
    case v               => trap(TypeMismatch(TInteger, v.vtype))
  }

  /** Evaluate the given expression as a int (interpeted as boolean), or halt with an error. */
  def evalBool(e: Expr): Op[Boolean] = evalInt(e).map(_ != 0)

}

