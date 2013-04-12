package eg.basic.interp

import scalaz.syntax.monad._
//import scalaz.syntax.bind._
import scalaz.syntax.std.boolean._

import eg.basic._
import eg.basic.ASTStatement._
import eg.basic.Variants._

// high-level statements
object StatementMachine extends StateMachine with LLMachine with ExprMachine {

  // All the IO is here, so in theory step can mix in another transformer for IO

  def input(isString: Boolean) = unit {
    def read: Variant = {
      print("? ")
      val s = readLine
      if (isString) VString(s)
      else try {
        Variant(s.toDouble)
      } catch {
        case _: NumberFormatException =>
          println("?REDO")
          read
      }
    }
    read
  }

  def output(v: Variant): Op[Unit] = unit(v).map {
    case VString(s) => s
    case VNumber(e) => e.fold(_.toString, _.toString)
  } map (println)

  def step: Op[Unit] = gets(s => s.p(s.pc)) >>= {

    case Let(k, e)       => (eval(e) >>= bind(k)) >> advance
    case Print(e)        => (eval(e) >>= output) >> advance
    case Goto(e)         => evalInt(e) >>= jmp
    case Gosub(e)        => evalInt(e) >>= jsr
    case Return          => ret
    case Input(p, k)     => (output(VString(p)) >> input(k.name.endsWith("$")) >>= bind(k)) >> advance
    case If(e1, e2)      => evalBool(e1).ifM(evalInt(e2) >>= jmp, advance)
    case Next(s: Symbol) => nextFor(s)
    case End             => end

    // Setting up a FOR loop has a few steps
    case For(s, e1, e2) =>
      for {
        i <- evalInt(e1)
        j <- evalInt(e2)
        _ <- bind(s)(Variant(i))
        n <- gets(s => s.next(s.pc))
        _ <- initFor(s, n, j)
        _ <- advance
      } yield ()

  }

}

