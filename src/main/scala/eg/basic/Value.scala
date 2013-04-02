package eg.basic

object Value {

  // Values
  sealed trait Val[T] {
    def get: T
    override def toString = get.toString
  }
  case class VStr(get: String) extends Val[String]
  case class VInt(get: Int) extends Val[Int]
  case class VReal(get: Float) extends Val[Float]

}