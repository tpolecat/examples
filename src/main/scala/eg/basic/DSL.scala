package eg.basic

import language.implicitConversions
import scala.collection.SortedMap
import ASTExpr._
import ASTStatement._
import Variants._

trait DSL {

  // This is the only var in the program. Not sure how to get rid of it.
  var prog: Program = SortedMap()

  // Line numbers
  implicit class pimpInt(pc: Int) {

    // Update our program. Impure, sorry.
    private def add(s: Statement) = prog = prog + (pc -> s)

    // Simple statements
    def END = add(End)
    def RETURN = add(Return)
    def PRINT(e: SExpr) = add(Print(e.a))
    def GOTO(n: Int) = add(Goto(Lit(Variant(n))))
    def GOSUB(n: Int) = add(Gosub(Lit(Variant(n))))
    def NEXT(s: Symbol) = add(Next(s))

    // INPUT "WHAT IS YOUR NAME" AS N
    def INPUT(prompt: String) = new {
      def AS(s: Symbol) = add(Input(prompt, s))
    }

    // LET A := 3
    def LET(s: Symbol) = new {
      def :=(e: SExpr) = add(Let(s, e.a))
    }

    // FOR S IN 1 TO 10
    def FOR(s: Symbol) = new {
      def IN(i: SExpr) = new {
        def TO(j: SExpr) = add(For(s, i.a, j.a))
      }
    }

    def IF(c: SExpr) = new {
      def THEN(n: Int) = add(If(c.a, Lit(Variant(n))))
    }

  }

  // Library functions
  // TODO: more of these, better
  def TAB(n: Int) = " " * n // this is actually syntax in the spec ... TODO: fix this
  def INT(e: SExpr) = SExpr(Int0(e.a))

  // Expressions
  case class SExpr(a: Expr) {
    def =?(b: SExpr): SExpr = SExpr(Bin(a, EQ, b.a))
    def <>(b: SExpr): SExpr = SExpr(Bin(a, NE, b.a))
    def <(b: SExpr): SExpr = SExpr(Bin(a, LT, b.a))
    def >(b: SExpr): SExpr = SExpr(Bin(a, GT, b.a))
    def <=(b: SExpr): SExpr = SExpr(Bin(a, LE, b.a))
    def >=(b: SExpr): SExpr = SExpr(Bin(a, GE, b.a))
    def +(b: SExpr): SExpr = SExpr(Bin(a, Plus, b.a))
    def -(b: SExpr): SExpr = SExpr(Bin(a, Minus, b.a))
    def *(b: SExpr): SExpr = SExpr(Bin(a, Times, b.a))
    def /(b: SExpr): SExpr = SExpr(Bin(a, Div, b.a))
  }

  // Variables and Literals
  implicit def Sym2SExpr(s: Symbol) = SExpr(Var(s))
  implicit def Str2SExpr(s: String) = SExpr(Lit(Variant(s)))
  implicit def Int2SExpr(n: Int) = SExpr(Lit(Variant(n)))
  implicit def Ibl2SExpr(d: Double) = SExpr(Lit(Variant(d)))

  // Numeric variables
  val A = 'A; val B = 'B; val C = 'C; val D = 'D; val E = 'E; val F = 'F; val G = 'G;
  val H = 'H; val I = 'I; val J = 'J; val K = 'K; val L = 'L; val M = 'M; val N = 'N;
  val O = 'O; val P = 'P; val Q = 'Q; val R = 'R; val S = 'S; val T = 'T; val U = 'U;
  val V = 'V; val W = 'W; val X = 'X; val Y = 'Y; val Z = 'Z; ;

  // String variables
  val A$ = 'A$; val B$ = 'B$; val C$ = 'C$; val D$ = 'D$; val E$ = 'E$; val F$ = 'F$; val G$ = 'G$;
  val H$ = 'H$; val I$ = 'I$; val J$ = 'J$; val K$ = 'K$; val L$ = 'L$; val M$ = 'M$; val N$ = 'N$;
  val O$ = 'O$; val P$ = 'P$; val Q$ = 'Q$; val R$ = 'R$; val S$ = 'S$; val T$ = 'T$; val U$ = 'U$;
  val V$ = 'V$; val W$ = 'W$; val X$ = 'X$; val Y$ = 'Y$; val Z$ = 'Z$;

}