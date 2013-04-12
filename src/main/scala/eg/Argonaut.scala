package eg

import scalaz._
import Scalaz._
import argonaut._
import Argonaut._
import scala.concurrent.future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success
import scala.util.Failure

// from http://argonaut.io/doc/quickstart/ 
object ArgonautTest extends App {

  case class Tweets(page: Int, next: Option[String], query: String, results: List[Tweet])

  object Tweets {

    // Simple decoder definition. Codec defined by inferenced types.
    implicit def TweetsDecodeJson: DecodeJson[Tweets] =
      jdecode4L(Tweets.apply)("page", "next_page", "query", "results")

  }

  case class Tweet(createdAt: String, user: String, text: String, retweets: Int)

  object Tweet {

    // Complete decoder definition. Manually using zipper to traverse.
    implicit def TweetDecodeJson: DecodeJson[Tweet] =
      DecodeJson { c =>
        for {
          createdAt <- (c --\ "created_at").jdecode[String]
          user <- (c --\ "from_user").jdecode[String]
          text <- (c --\ "text").jdecode[String]
          retweets <- (c --\ "metadata" --\ "recent_retweets").jdecode[Option[Int]]
        } yield Tweet(createdAt, user, text, retweets.getOrElse(0))
      }

  }

  
  
  val p = future {
    val url = "http://search.twitter.com/search.json?q=tpolecat"
    scala.io.Source.fromURL(url).mkString // Don't do this at home
  }

  p.onComplete {
    case Success(string) =>

      println(string)
      
      val x = string.decode[Tweets]
      x match {
        case -\/(\/-((s, h))) => println(s, h.toList)
        case z => println(z)
      }
      
      // Parse and decode the string to your data type using its codec.
      val message = string.decodeOption[Tweets] match {
        case None => "Could not decode Tweets."
        case Some(tweets) => "Decoded %d tweets, with %d retweets:".format(
          tweets.results.size,
          tweets.results.map(_.retweets).sum) ++ tweets.results.mkString("\n", "\n\t", "\n")
      }

      println(message)

    case Failure(e) => e.printStackTrace()

  }

  println("waiting...")

  Thread.sleep(10000)
  
}