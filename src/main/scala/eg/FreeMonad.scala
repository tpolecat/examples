package eg

import scala.language.higherKinds
import scala.annotation.tailrec
import scalaz._
import scalaz.Free._

sealed trait MapOp[+A]
case class Put[A](k: String, v: String, q: Option[String] => A) extends MapOp[A]
case class Get[A](k: String, q: Option[String] => A) extends MapOp[A]
case class Del[A](k: String, q: Option[String] => A) extends MapOp[A]

object MapOp {
  implicit val MapOpFunctor: Functor[MapOp] = new Functor[MapOp] {
    def map[A, B](kv: MapOp[A])(f: A => B) = kv match {
      case Put(k, v, q) => Put(k, v, f compose q)
      case Get(k, q)    => Get(k,    f compose q)
      case Del(k, q)    => Del(k,    f compose q)
    }
  } 
}

object FreeMapOp {

  type Action[A] = Free[MapOp, A]

  def put(k: String, v: String): Action[Option[String]] = Suspend(Put(k, v, Return(_)))
  def get(k: String): Action[Option[String]] = Suspend(Get(k, Return(_)))
  def del(k: String): Action[Option[String]] = Suspend(Del(k, Return(_)))

  implicit class RunnableMapAction[A](a: Action[A]) {
    
    // CAUTION: Unsafe operation. Run once only.
    @tailrec final def runJMap(m: java.util.HashMap[String, String]): A =
      a.resume match { // N.B. resume.fold() doesn't permit TCO
        case -\/(a) => a match {
          case Put(k, v, q) => q(Option(m put   (k, v))) runJMap m
          case Get(k,    q) => q(Option(m get    k))     runJMap m
          case Del(k,    q) => q(Option(m remove k))     runJMap m
        }
        case \/-(a) => a
      }
  
  }

}

object Main {
    
  def main(args: Array[String]) {
    val conf = new java.util.HashMap[String, String]
    conf put ("ak", "av")
    conf put ("bk", "bv")
    conf put ("ck", "cv")

    import FreeMapOp._

    val a0 = get("ak")
    val a1 = get("akX")
    val a2 = del("akX")
    val a3 = put("akX", "avX")
    val a4 = get("ak")
    val a5 = get("akX")
    val a6 = del("akX")
    val a7 = get("akX")
    val a8 = get("ak")
    val a9 = put("ak", "AV")

    val q: Free[MapOp, List[Option[String]]] =
      for {
        e0 <- a0
        e1 <- a1
        e2 <- a2
        e3 <- a3
        e4 <- a4
        e5 <- a5
        e6 <- a6
        e7 <- a7
        e8 <- a8
        e9 <- a9
      } yield List(e0, e1, e2, e3, e4, e5, e6, e7, e8, e9)

    val r = q runJMap conf

    r.zipWithIndex foreach {
      case (i, j) => println(j + ": " + i)
    }
  }
}

