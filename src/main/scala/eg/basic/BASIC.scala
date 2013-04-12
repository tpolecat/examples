package eg.basic

import scalaz._
import ASTExpr._
import ASTStatement._
import interp.StatementMachine._
import scala.annotation.tailrec

trait BASIC extends DSL {

  @tailrec
  private def go(r: Running): Halted =
    step(r) match { // N.B. fold works but isn't tail recursive
      case -\/(a)      => a
      case \/-((r, _)) => go(r)
    }

  def RUN = run()

  /** Run the program and report errors. */
  def run(): Unit =
    for {
      n <- prog.keys.headOption
      e <- go(Running(prog, n)).error
    } System.err.println(e)

}


