package eg.basic

import language.implicitConversions

trait DSL {

  import Value._
  import Syntax._

  protected var prog: Program = Map.empty

  implicit class pimpInt(pc: Int) {
    
    // Update our program. Impure, sorry.
    private def add(s: Statement) = prog = prog + (Line(pc) -> s)

    // Simple statements
    def END              = add(End)
    def RETURN           = add(Return)
    def PRINT(n: Int)    = add(Print(Lit(VInt(n))))
    def PRINT(s: Symbol) = add(Print(Var(s)))
    def PRINT(s: String) = add(Print(Lit(VStr(s))))
    def GOTO(n: Int)     = add(Goto(Line(n)))
    def GOSUB(n: Int)    = add(Gosub(Line(n)))
    def NEXT(s: Symbol)  = add(Next(Var(s)))

    // LET A := 3
    def LET(s: Symbol) = new {
      def :=(n: Int)    = add(Let(Var(s), Lit(VInt(n))))
      def :=(p: Symbol) = add(Let(Var(s), Var(p)))
    }

    // FOR S IN 1 TO 10
    def FOR(s: Symbol) = new {
      def IN(i: Int) = new {
        def TO(j: Int) = add(For(Var(s), i, j))
      }
    }

  }

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

}