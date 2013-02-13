package eg

import scalaz._
import Scalaz._
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.JavaTokenParsers
import Quote._

sealed trait JExpr

case object JNull extends JExpr {
  override def toString = "null"
}

case class JBool(value: Boolean) extends JExpr {
  override def toString = value.toString
}

case class JStr(value: String) extends JExpr {
  override def toString = quote(value)
}

case class JNum(value: Double) extends JExpr {
  override def toString = value.toString
}

case class JVec(values: List[JExpr]) extends JExpr {
  override def toString = values.mkString("[", ", ", "]")
}

case class JMap(values: Map[String, JExpr]) extends JExpr {
  override def toString = values.map {
    case (k, v) => "%s : %s".format(quote(k), v)
  }.mkString("{", ", ", "}")
}

object JExpr {

  def parse(s: String) = Parser.go(s).toOption // for now

  object Parser extends JavaTokenParsers {

    def go(s: String): Validation[String, JExpr] = parseAll(expr, s) match {
      case Success(s, _) => s.success
      case Failure(f, _) => f.fail
      case Error(f, _)   => f.fail
    }

    def expr: Parser[JExpr] =
	  "null"               ^^^      JNull             |
	  "true"               ^^^      JBool(true)       |
	  "false"              ^^^      JBool(false)      |
	  stringLiteral        ^^ (s => JStr(unquote(s))) |
	  floatingPointNumber  ^^ (s => JNum(s.toDouble)) |
	  list("{", pair, "}") ^^ (l => JMap(l.toMap))    |
	  list("[", expr, "]") ^^ (l => JVec(l)) 
	  
	// list of p bracketed by bra and ket
    def list[A](bra: String, p: Parser[A], ket: String) = bra ~> repsep(p, ",") <~ ket

    // k:v pair, where k is a string and v is any expr
    def pair: Parser[(String, JExpr)] = 
      stringLiteral ~ ":" ~ expr ^^ { case k ~ ":" ~ v => (unquote(k), v) }

    // Overridden to fix a bug ... \" doesn't work in the superclass
    override def stringLiteral: Parser[String] =
      """"([^"\p{Cntrl}\\]|\\[\\/bfnrt\"]|\\u[a-fA-F0-9]{4})*"""".r

  }

}

