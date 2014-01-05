package eg

import java.io._
import java.net._

import scalaz._
import scalaz.syntax.monad._
import scalaz.effect._
import scalaz.effect.IO._

object Sock extends SafeApp {

  val port = 8066

  override val runc: IO[Unit] = 
    for {
      s <- IO(new ServerSocket(port))
      _ <- putStrLn(s"Listening on port $port")
      c <- IO(s.accept)
      _ <- IO(s.close) // only accept one client!
      _ <- IO(new Thread { override def run = serve(c).unsafePerformIO } .start)
      _ <- IO("I will exit when the client disconnects.")
    } yield ()

  def serve(s: Socket): IO[Unit] =
    for {
      _ <- putStrLn(s"Connection from $s")
      i <- IO(new BufferedReader(new InputStreamReader(s.getInputStream)))
      o <- IO(new PrintWriter(s.getOutputStream(), true))
      _ <- IO(o.println("Welcome, type 'exit' to quit."))
      _ <- loop(s, i, o, 1)
    } yield ()

  def loop(s: Socket, i: BufferedReader, o: PrintWriter, state: Int): IO[Unit] =
    IO(Option(i.readLine)) >>= { 
      case None         => putStrLn("Disconnected!")
      case Some("exit") => IO(o.println("Bye.")) >> IO(s.close)
      case Some(msg)    => IO(o.println(s"$state. $msg")) >> loop(s, i, o, state + 1)
    }

}
