package eg.basic

import language.implicitConversions
import scala.collection.SortedMap

trait DSL {

  import ASTExpr._
  import ASTStatement._
  import Variants._

  protected var prog: Program = SortedMap()

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
  def TAB(n: Int) = " " * n
  def INT(e: Expr) = Int0(e)

  case class SExpr(a: Expr) {
    def =?(b: SExpr): SExpr = SExpr(Bin(a, EQ, b.a))
    def <>(b: SExpr): SExpr = SExpr(Bin(a, NE, b.a))
    def <(b: SExpr): SExpr = SExpr(Bin(a, LT, b.a))
    def >(b: SExpr): SExpr = SExpr(Bin(a, GT, b.a))
    def +(b: SExpr): SExpr = SExpr(Bin(a, Plus, b.a))
    def -(b: SExpr): SExpr = SExpr(Bin(a, Minus, b.a))
  }

  implicit def Sym2SExpr(s: Symbol) = SExpr(Var(s))
  implicit def Str2SExpr(s: String) = SExpr(Lit(Variant(s)))
  implicit def Int2SExpr(n: Int) = SExpr(Lit(Variant(n)))

  def INT(e: SExpr) = SExpr(Int0(e.a))

  // So we can use common variables without quoting
  protected val A = 'A
  protected val B = 'B
  protected val C = 'C
  protected val D = 'D
  protected val E = 'E
  protected val F = 'F
  protected val G = 'G
  protected val H = 'H
  protected val I = 'I
  protected val J = 'J
  protected val K = 'K
  protected val L = 'L
  protected val M = 'M
  protected val N = 'N
  protected val O = 'O
  protected val P = 'P
  protected val Q = 'Q
  protected val R = 'R
  protected val S = 'S
  protected val T = 'T
  protected val U = 'U
  protected val V = 'V
  protected val W = 'W
  protected val X = 'X
  protected val Y = 'Y
  protected val Z = 'Z

  protected val A$ = 'A$
  protected val B$ = 'B$
  protected val C$ = 'C$
  protected val D$ = 'D$
  protected val E$ = 'E$
  protected val F$ = 'F$
  protected val G$ = 'G$
  protected val H$ = 'H$
  protected val I$ = 'I$
  protected val J$ = 'J$
  protected val K$ = 'K$
  protected val L$ = 'L$
  protected val M$ = 'M$
  protected val N$ = 'N$
  protected val O$ = 'O$
  protected val P$ = 'P$
  protected val Q$ = 'Q$
  protected val R$ = 'R$
  protected val S$ = 'S$
  protected val T$ = 'T$
  protected val U$ = 'U$
  protected val V$ = 'V$
  protected val W$ = 'W$
  protected val X$ = 'X$
  protected val Y$ = 'Y$
  protected val Z$ = 'Z$

}