package eg

import scala.language.higherKinds
import scala.annotation.tailrec
import scalaz._
import scalaz.syntax.monad._
import scalaz.syntax.id._
import scalaz.effect._

// adapted from an example by @larsrh 
object FreeMonad extends App {

  // Simple algebra of terminal operations
  sealed trait TerminalOp[A]
  object TerminaIO {
    case object ReadLine extends TerminalOp[String]
    case class  WriteLine(value: String) extends TerminalOp[Unit]
  }
  import TerminaIO._

  // Free monad over the free functor of TerminalOp
  type TerminalIO[A] = Free.FreeC[TerminalOp, A]

  // It's a monad (but the instance is not inferrable)
  implicit val monadTerminalIO: Monad[TerminalIO] =
    Free.freeMonad[({type λ[α] = Coyoneda[TerminalOp, α]})#λ]

  // Smart constructors in TerminalIO
  def readLine: TerminalIO[String] = Free.liftFC(ReadLine)
  def writeLine(s: String): TerminalIO[Unit] = Free.liftFC(WriteLine(s))

  // Natural transformation to IO
  def terminalToIO: TerminalOp ~> IO = new (TerminalOp ~> IO) {
    def apply[A](t: TerminalOp[A]): IO[A] = t match {
      case ReadLine     => IO.readLn
      case WriteLine(s) => IO.putStrLn(s)
    }
  }

  // Natural transformation to a Mock IO threaded through State
  case class Mock(in: List[String], out: List[String])
  object Mock {
    def read(mock: Mock): (Mock, String) = mock.in match {
      case Nil    => (mock, "")
      case h :: t => (mock.copy(in = t), h)
    }
    def write(value: String)(mock: Mock): Mock =
      mock.copy(out = value :: mock.out)
  }
  type MockState[A] = State[Mock, A]
  def terminalToState: TerminalOp ~> MockState = new (TerminalOp ~> MockState) {
    def apply[A](t: TerminalOp[A]): MockState[A] = t match {
      case ReadLine     => State(Mock.read)
      case WriteLine(s) => State.modify(Mock.write(s))
    }
  }

  // An example program
  val program = for {
    x <- readLine
    y <- readLine
    _ <- writeLine(x + " " + y)
  } yield ()

  // Run it with the mock
  val init = Mock(in = List("Hello", "World"), out = List())
  println(Free.runFC(program)(terminalToState).exec(init).out)

  // Run it for real
  Free.runFC(program)(terminalToIO).unsafePerformIO

}

