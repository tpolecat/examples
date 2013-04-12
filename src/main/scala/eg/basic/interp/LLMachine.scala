package eg.basic.interp

import scalaz.-\/
import scalaz.StateT
import scalaz.syntax.monad._
import scalaz.syntax.std.boolean._
import eg.basic._
import eg.basic.Errors._
import eg.basic.Variants._

/** Low-level machine that doesn't handle any kind of syntax. */
trait LLMachine { this: StateMachine =>

  /** Halt the machine (with an `Error` if abnormal termination) **/
  def halt[A](oe: Option[Error]): Op[A] =
    StateT[Answer, Running, A](s => -\/(Halted(s, oe)))

  /** Halt the machine with the specified `Error`. */
  def trap[A](e: Error): Op[A] = halt[A](Some(e))

  /** Halt the machine normally. */
  def end: Op[Unit] = halt(None)

  /** Jump to the specified line, or halt with `UnknownLine`. */
  def jmp(n: Int): Op[Unit] =
    gets(_.p.isDefinedAt(n)).ifM(modify(_.copy(pc = n)), trap(UndefinedLine(n)))

  /** Jump to the subroutine at the specified line, or halt with `UnknownLine`. */
  def jsr(n: Int): Op[Unit] =
    for {
      m <- gets(s => s.next(s.pc))
      _ <- jmp(n) // this will check that `n` is valid
      _ <- modify(s => s.copy(stack = m :: s.stack))
    } yield ()

  /**
   * Return from a subroutine, halting normally if GOSUB was the last statement, or with `ReturnWithoutGosub` if
   * stack is empty.
   */
  def ret: Op[Unit] =
    gets(_.stack) >>= {
      case Some(n) :: ns => modify(_.copy(pc = n, stack = ns))
      case None :: _     => end // GOSUB was last statement in program
      case Nil           => gets(_.pc).map(ReturnWithoutGosub) >>= trap
    }

  /** Advance to the next line, or halt normally. */
  def advance: Op[Unit] =
    gets(s => s.next(s.pc)) >>= (_.fold(end)(n => modify(_.copy(pc = n))))

  val DefaultNumeric = Variant(0)
  val DefaultString = Variant("")
    
  /** Look up a variable, returning `0` or `""` if the variable is unbound. */
  def lookup(s: Symbol): Op[Variant] =
    gets(_.bindings.get(s)).map(_.getOrElse(if (s.isStringVariable) DefaultString else DefaultNumeric))

  /** Bind a value. */
  def bind(k: Symbol)(v: Variant): Op[Unit] = {
    val ok = modify(s => s.copy(bindings = s.bindings + (k -> v)))
    v.vtype match {
      case TString => if (k.isStringVariable) ok else trap(TypeMismatch(TNumeric, TString))
      case t       => if (k.isStringVariable) trap(TypeMismatch(TString, t)) else ok
    }
  }

  /** Initiate a `for` loop, given the binding variable, the body entry point, and max value. */
  def initFor(k: Symbol, entry: Option[Int], max: Int): Op[Unit] =
    entry match {
      case None    => end // FOR was the last statement
      case Some(n) => modify(s => s.copy(loops = (k, n, max) :: s.loops))
    }

  /** Increment a loop variable and return the jump point. */
  def nextFor(k: Symbol): Op[Unit] =
    gets(_.loops) >>= {
      case (`k`, n, max) :: ls => lookup(k) >>= {
        case VNumber(-\/(i)) if i < max  => bind(k)(VNumber(-\/(i + 1))) >> jmp(n)
        case VNumber(-\/(i)) if i >= max => advance
        case VNumber(_)                  => trap(TypeMismatch(TInteger, TDouble))
        case VString(_)                  => trap(TypeMismatch(TInteger, TString))
      }
      case _ => gets(_.pc).map(NextWithoutFor) >>= trap
    }

}
