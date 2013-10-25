package eg

import scalaz.Tag
import scalaz.@@

object Tags extends App {

  // Tagged types are unboxed so there is no runtime cost for tagging.
  // An A @@ B is also an A (so you can treat it as untagged)
  
  // A marker trait
  sealed trait Ciphertext
  
  def encode(s: String): String @@ Ciphertext = 
    Tag(s.map(c => (c + 13).toChar))
  
  def decode(s: String @@ Ciphertext): String =
    s.map(c => (c - 13).toChar)

  val secret = encode("eskimo")
  
  decode(secret) // ok; secret is tagged  
//decode("foo")  // doesn't compile; we can only decode a tagged string
 
  println(encode("foobar"))
  println(decode(encode("foobar")))
    
}