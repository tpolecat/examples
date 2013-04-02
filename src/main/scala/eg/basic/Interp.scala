package eg.basic

import scala.language.implicitConversions
import scala.annotation.tailrec
import scala.Ordering.Implicits._

trait Interp {

  import Value._
  import Syntax._

  // A loop has an entry point, a step, and a limit
  private case class Loop(entry: Line, step: Int, limit: Int)

  private type Bindings = Map[Symbol, Val[_]]
  private type LoopState = Map[Symbol, Loop]

  // Our runtime state
  private case class State(
    p: Program,
    pc: Line,
    bindings: Bindings,
    loops: LoopState,
    stack: List[Option[Line]]) {

    // Sanity check
    require(p.isDefinedAt(pc))

    // Reduce an expression
    private def eval(e: Expr): Val[_] = e match {
      case Var(s) => bindings.get(s).getOrElse(VInt(0))
      case Lit(v) => v
    }

    // New state with new binding
    private def bind(v: Var, e: Expr): State = bind(v, eval(e))
    private def bind(v: Var, a: Val[_]): State = copy(bindings = bindings + (v.s -> a))

    // Report an error and return the next state (which is None; we halt)
    private def err(s: String): Option[State] = {
      println("Error at line %d: %s".format(pc, s))
      None
    }

    // What's the next line number:
    private def nextPC: Option[Line] = {
      val gt = p.keys.filter(_ > pc) // yeah, slow
      if (gt.isEmpty) None else Some(gt.min)
    }

    // What's the next state, if we just continue to the next line?
    private def next: Option[State] =
      nextPC.map(pc => copy(pc = pc))

    // What's the next state, if we try to jump to the specified line?
    private def goto(n: Line): Option[State] =
      if (p.isDefinedAt(n)) Some(copy(pc = n))
      else err("invalid line number: %d".format(n))

    // What's the next state, if construct a new loop?
    private def loop(v: Var, i: Int, j: Int): Option[State] =
      if (loops.isDefinedAt(v.s)) err("FOR %s is already defined at line %d.".format(v, loops(v.s).entry))
      else nextPC.flatMap(pc => bind(v, VInt(i)).copy(loops = loops + (v.s -> Loop(pc, 1, j))).next)

    // What's the next state, if we go to the next loop index?
    private def next(v: Var): Option[State] =
      loops.get(v.s) match {
        case Some(Loop(pc, s, j)) => eval(v) match {
          case VInt(i) if (i < j) => bind(v, VInt(i + s)).goto(pc)
          case _                  => copy(loops = loops - v.s).next
        }
        case None => err("NEXT %s without FOR".format(v))
      }

    // Our major transition function. Find the next state based on the current step.
    def step: Option[State] = p(pc) match {

      case Let(v, e)    => bind(v, e).next
      case Print(e)     => { println(eval(e)); next }
      case Goto(n)      => goto(n)
      case Gosub(n)     => goto(n).map(_.copy(stack = nextPC :: stack))
      case End          => None
      case For(v, i, j) => loop(v, i, j)
      case Next(v)      => next(v)

      case Return => stack match {
        case Some(n) :: _ => goto(n).map(_.copy(stack = stack.tail))
        case None :: _    => None // GOSUB was last statement; RETURN == END
        case Nil          => err("RETURN without GOSUB")
      }

    }

  }

  private object State {
    def initial(p: Program): State = State(p, p.keys.min, Map(), Map(), Nil)
  }

  protected def run(p: Program): Unit =
    if (!p.keys.isEmpty)
      run(State.initial(p))

  // Our interpreter is tail-recursive; our programs can run forever!
  @tailrec private def run(s: State): Unit =
    s.step match {
      case Some(s) => run(s)
      case None    => ()
    }

}



