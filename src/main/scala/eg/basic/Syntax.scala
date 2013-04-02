package eg.basic

object Syntax {

  import Value._
  
  // Line numbers
  case class Line(n: Int) extends AnyVal
  object Line {
    implicit val ord: Ordering[Line] = Ordering.by(_.n)
  }

  // Expressions.
  sealed trait Expr
  case class Var(s: Symbol) extends Expr { override def toString = s.name }
  case class Lit[T](v: Val[T]) extends Expr { override def toString = v.toString }

  // Statements
  sealed trait Statement
  case class Let(v: Var, e: Expr) extends Statement
  case class Print(e: Expr) extends Statement
  case class Goto(n: Line) extends Statement
  case class Gosub(n: Line) extends Statement
  case object Return extends Statement
  case object End extends Statement
  case class For(v: Var, i: Int, j: Int) extends Statement
  case class Next(v: Var) extends Statement

  // A program is just a map from line number to statement.
  type Program = Map[Line, Statement]

}