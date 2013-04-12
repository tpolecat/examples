package eg

object Unapply extends App {

  class Lit(val s:String)
  
  object LInt {
    def unapply(lit:Lit) = try { Some(lit.s.toInt) } catch { case nfe:NumberFormatException => None }
  }
  
  object LStr {
    def unapply(lit:Lit) = Some(lit.s)
  }
  
  val LInt(n) = new Lit("123")
  val LStr(s) = new Lit("foo")
  
  println(n)
  println(s)
  
}