package eg.basic

import scala.collection.SortedMap

object ASTStatement {

  import ASTExpr._
  
  // Statements
  sealed trait Statement
  case class Let(s: Symbol, e: Expr) extends Statement
  case class Print(e: Expr) extends Statement
  case class Goto(e: Expr) extends Statement
  case class Gosub(e: Expr) extends Statement
  case object Return extends Statement
  case object End extends Statement
  case class For(s:Symbol, e1: Expr, e2: Expr) extends Statement
  case class Next(s:Symbol) extends Statement
  case class Input(prompt: String, s: Symbol) extends Statement
  case class If(cond: Expr, e: Expr) extends Statement

  // A program is just a map from line number to statement.
  type Program = SortedMap[Int, Statement]

}