package eg.basic

object Errors {

  import Variants._

  sealed abstract class Error(
    val code: Int,
    val abbrev: String,
    val error: String) {
    override def toString = s"$abbrev $error"
  }

  case class NextWithoutFor(n: Int) extends Error(1, "NF", "Next Without For: " + n)
  case class ReturnWithoutGosub(n: Int) extends Error(3, "RG", "Return Without Gosub: " + n)
  case class UndefinedLine(n: Int) extends Error(8, "UL", "Undefined Line: " + n)
  case class TypeMismatch(expected: Type, actual: Type) extends Error(13, "TM", s"Type Mismatch: Expected $expected, found $actual.")

}