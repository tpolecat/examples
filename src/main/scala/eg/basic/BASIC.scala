package eg.basic

import scalaz._
import ASTExpr._
import ASTStatement._
import interp.StatementMachine

trait BASIC extends DSL {

  import StatementMachine._

  def RUN = prog.keys.headOption.foreach { pc =>
    go(Running(prog, pc, Nil, Map())).error.foreach(System.err.println)
  }

  def run(): Unit = RUN

  private def go(r: Running): Halted =
    step(r) match {
      case -\/(a)      => a
      case \/-((r, _)) => go(r)
    }

}

