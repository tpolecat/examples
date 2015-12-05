package eg

import scalaz.effect.IO
import scalaz.effect.IO._
import scalaz._
import scalaz.Scalaz._
import scalaz.effect.SafeApp
import FreeSTM._

/**
 *
 * Adapted from "Beautiful concurrency", Simon Peyton Jones, Microsoft Research, Cambridge
 * http://research.microsoft.com/en-us/um/people/simonpj/papers/stm/beautiful.pdf
 *
 * From Section 4: "The Santa Claus Problem"
 *
 *   Santa repeatedly sleeps until wakened by either all of his nine reindeer, back from their holidays, or by a
 *   group of three of his ten elves. If awakened by the reindeer, he harnesses each of them to his sleigh, delivers
 *   toys with them and finally unharnesses them (allowing them to go off on holiday). If awakened by a group of elves,
 *   he shows each of the group into his study, consults with them on toy R&D and finally shows them each out (allowing
 *   them to go back to work). Santa should give priority to the reindeer in the case that there is both a group of
 *   elves and a group of reindeer waiting.
 *
 * Notes:
 *
 *   This adaptation maps the original implementation as closely as is reasonable, so it's more verbose in places than
 *   what I would normally write.
 *
 */
object Santa extends SafeApp with ExtraOps {

  // 4.1 Reindeer and elves

  def meetInStudy(id: Int): IO[Unit] =
    putStrLn(s"Elf $id meeting in the study")

  def deliverToys(id: Int): IO[Unit] =
    putStrLn(s"Reindeer $id delivering toys")

  def helper1(group: Group, do_task: IO[Unit]): IO[Unit] =
    for {
      x <- joinGroup(group)
      (in_gate, out_gate) = x // N.B. we have to destructure via a temp :-\
      _ <- passGate(in_gate)
      _ <- do_task
      _ <- passGate(out_gate)
    } yield ()

  def elf1(gp: Group, id: Int): IO[Unit] =
    helper1(gp, meetInStudy(id))

  def reindeer1(gp: Group, id: Int): IO[Unit] =
    helper1(gp, deliverToys(id))

  // 4.2 Gates and Groups

  case class Gate(n: Int, tv: TVar[Int])

  def newGate(n: Int): STM[Gate] =
    for {
      tv <- newTVar(0)
    } yield Gate(n, tv)

  def passGate(g: Gate): IO[Unit] =
    atomically {
      for {
        n_left <- readTVar(g.tv)
        _ <- check(n_left > 0)
        _ <- writeTVar(g.tv, (n_left - 1))
      } yield ()
    }

  // operateGate :: Gate -> IO () 
  def operateGate(g: Gate): IO[Unit] =
    for {
      _ <- atomically(writeTVar(g.tv, g.n))
      _ <- atomically {
        for {
          n_left <- readTVar(g.tv)
          _ <- check(n_left == 0)
        } yield ()
      }
    } yield ()

  case class Group(n: Int, tv: TVar[(Int, Gate, Gate)])

  def newGroup(n: Int): IO[Group] =
    atomically {
      for {
        g1 <- newGate(n)
        g2 <- newGate(n)
        tv <- newTVar((n, g1, g2))
      } yield Group(n, tv)
    }

  def joinGroup(g: Group): IO[(Gate, Gate)] =
    atomically {
      for {
        x <- readTVar(g.tv)
        (n_left, g1, g2) = x
        _ <- check(n_left > 0)
        _ <- writeTVar(g.tv, (n_left - 1, g1, g2))
      } yield (g1, g2)
    }

  def awaitGroup(g: Group): STM[(Gate, Gate)] =
    for {
      x <- readTVar(g.tv)
      (n_left, g1, g2) = x
      _ <- check(n_left == 0)
      new_g1 <- newGate(g.n)
      new_g2 <- newGate(g.n)
      _ <- writeTVar(g.tv, (g.n, new_g1, new_g2))
    } yield (g1, g2)

  // 4.3 The main program

  override def runc: IO[Unit] =
    for {
      elf_group <- newGroup(3)
      _ <- (1 |-> 10).traverse(elf(elf_group, _))
      rein_group <- newGroup(9)
      _ <- (1 |-> 9).traverse(reindeer(rein_group, _))
      _ <- santa(elf_group, rein_group).replicateM(100) // don't run forever, it gets boring
      _ <- IO.putStrLn("Ok stopping ... could keep going but you get the idea")
    } yield ()

  def elf(gp: Group, id: Int): IO[ThreadId] =
    forkIO {
      forever {
        for {
          _ <- elf1(gp, id)
          _ <- randomDelay
        } yield ()
      }
    }

  def randomDelay: IO[Unit] =
    for {
      waitTime <- IO(scala.util.Random.nextInt(1000))
      _ <- IO(Thread.sleep(waitTime))
    } yield ()

  def reindeer(gp: Group, id: Int): IO[ThreadId] =
    forkIO {
      forever {
        for {
          _ <- reindeer1(gp, id)
          _ <- randomDelay
        } yield ()
      }
    }

  // 4.4 Implementing Santa

  def santa(elf_gp: Group, rein_gp: Group): IO[Unit] = {

    def chooseGroup(gp: Group, task: String): STM[(String, (Gate, Gate))] =
      for {
        gates <- awaitGroup(gp)
      } yield (task, gates)

    for {
      _ <- putStrLn("----------")
      x <- atomically {
        orElse(chooseGroup(rein_gp, "deliver toys"), chooseGroup(elf_gp, "meet in my study"))
      }
      (task, (in_gate, out_gate)) = x
      _ <- putStrLn(s"Ho! Ho! Ho! let’s $task")
      _ <- operateGate(in_gate)
      _ <- operateGate(out_gate)
    } yield ()

  }

}


// Some extra combinators used in the example
trait ExtraOps {

  type ThreadId = Long

  def check(b: => Boolean): STM[Unit] =
    b ? ().point[STM] | retry

  def forkIO(a: IO[Unit]): IO[ThreadId] =
    IO {
      val t = new Thread { // yes, really
        setDaemon(true)
        override def run: Unit = a.unsafePerformIO
      }
      t.start()
      t.getId()
    }

  def forever(act: IO[Unit]): IO[Unit] =
    act.flatMap(_ => forever(act)) // N.B. important to never yield, otherwise we leak heap

}
