package eg.basic.interp

import scalaz.\/
import scalaz.State
import scalaz.StateT

import eg.basic._
import eg.basic.Errors._
import eg.basic.Variants._
import eg.basic.ASTStatement._

trait StateMachine {

  /**
   * State of a running machine, with the program, program counter, stack, and bindings.
   */
  sealed case class Running(
      p: Program, 
      pc: Int, 
      stack: List[Option[Int]] = Nil, 
      bindings: Map[Symbol, Variant] = Map(),
      loops: List[(Symbol, Int, Int)] = Nil
      ) {
    
    require(p.isDefinedAt(pc)) // TODO: program should be a zipper, making this unnecessary

    /** Compute the next line number after `n`, if any. */
    def next(n: Int): Option[Int] = p.keys.filter(_ > n).headOption

  }

  /** State of a halted machine, with the last known running state and the `Error` that halted execution, if any. */
  sealed case class Halted(finalState: Running, error: Option[Error])

  /** Type alias for a disjunction of `Halted` and something else. */
  type Answer[+A] = Halted \/ A // Inference croaks if we use a type lambda for this in the definition of `Op`

  /**
   * The type of operations for our iterpreter. An operation consumes a `Running` state and returns either a new state
   * and the result, or a `Halted` state (optionally with an `Error`).
   */
  type Op[+A] = StateT[Answer, Running, A]

  // LIFTED STATE OPERATIONS

  /** Operation to return the current `Running` state. */
  def get: Op[Running] = State.get.lift

  /** Operation to return the current `Running` state, mapped to some type `A`. */
  def gets[A](f: Running => A): Op[A] = State.gets(f).lift

  /** Operation to modify the current `Running` state. */
  def modify(f: Running => Running): Op[Unit] = State.modify(f).lift

  /** Operation to replace the current `Running` state. */
  def put(s: Running): Op[Unit] = State.put(s).lift

  /** Operation that returns the passed value. */
  def unit[A](a: A): Op[A] = State.state(a).lift

}
