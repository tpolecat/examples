package eg.basic

import scalaz._
import ASTExpr._
import ASTStatement._
import interp.StatementMachine

trait BASIC extends DSL {

  import StatementMachine._

  def RUN = prog.keys.headOption.foreach { pc =>
    run(Running(prog, pc, Nil, Map())).error.foreach(System.err.println)
  }

  private def run(r: Running): Halted =
    step(r) match {
      case -\/(a)      => a
      case \/-((r, _)) => run(r)
    }

}

