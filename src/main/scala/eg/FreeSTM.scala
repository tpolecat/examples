package eg

import scalaz.effect.IO
import scalaz.effect.IO._
import scalaz._
import scalaz.Scalaz._
import scalaz.effect.SafeApp

/** 
 * Pure functional STM using a free monad to hide the underlying Scala STM. Refs and transaction 
 * tokens are not observable. The end result looks a lot like Haskell STM. See StmSanta.scala for 
 * a running example.
 */
object FreeSTM {
  import scalaz.Free.{ liftFC, runFC }
  import scala.concurrent.stm.{ retry => stmRetry, _ }

  // Transactional Variable
  final class TVar[A](private[FreeSTM] val ref: Ref[A])

  // Algebra of STM Operations (private)
  sealed trait Op[A]
  private object Op {
    case class  NewTVar[A](a: A) extends Op[TVar[A]]
    case class  ReadTVar[A](fa: TVar[A]) extends Op[A]
    case class  WriteTVar[A](fa: TVar[A], a: A) extends Op[Unit]
    case object Retry extends Op[Unit]
    case class  Delay[A](fa: () => A) extends Op[A]
  }
  import Op._

  // Free monad over free functor of Op
  type Coyo[A] = Coyoneda[Op, A]
  type STM[A]  = Free[Coyo, A]
  implicit def MonadSTM: Monad[STM] = Free.freeMonad[Coyo]

  // Smart Constructors
  def newTVar[A](a: A): STM[TVar[A]] = liftFC[Op, TVar[A]](NewTVar(a))
  def readTVar[A](r: TVar[A]): STM[A] = liftFC(ReadTVar(r))
  def writeTVar[A](r: TVar[A], a: A): STM[Unit] = liftFC[Op, Unit](WriteTVar(r, a))
  val retry: STM[Unit] = liftFC(Retry)

  // Interpret Op to Reader
  private type InTxnReader[A] = InTxn => A
  private val interpOp: Op ~> InTxnReader =
    new (Op ~> InTxnReader) {
      def apply[A](fa: Op[A]): InTxnReader[A] =
        fa match {
          case ReadTVar(fa)     => { implicit tx => fa.ref() }
          case WriteTVar(fa, a) => { implicit tx => fa.ref() = a }
          case Retry            => { implicit tx => stmRetry }
          case NewTVar(a)       => { implicit tx => new TVar(Ref(a)) }
          case Delay(fa)        => { implicit tx => fa() }
        }
    }

  // Interpret STM to Reader
  private def interpFC[A](a: STM[A]): InTxnReader[A] =
    runFC[Op, InTxnReader, A](a)(interpOp)

  // orElse combinator
  def orElse[A](a: STM[A], b: STM[A]): STM[A] =
    liftFC(Delay(() => atomic(interpFC(a)).orAtomic(interpFC(b))))

  // Lift to IO
  def atomically[A](a: STM[A]): IO[A] =
    IO(atomic(interpFC(a)))

}
