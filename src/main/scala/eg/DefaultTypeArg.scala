package eg

// Via Jon Pretty - @propensive
object DefaultTypeArg {

  // Define the following traits and companion object
  // It's in Rapture Core (https://github.com/propensive/rapture-core) if you don't want to
   
  trait LowPriorityDefaultsTo { implicit def fallback[T, S]: DefaultsTo[T, S] = null }
  object DefaultsTo extends LowPriorityDefaultsTo { implicit def defaultDefaultsTo[T]: DefaultsTo[T, T] = null }
  trait DefaultsTo[T, S]
   
  // Then, assuming we want to specify a default for a type class like `Namer`,
  case class Namer[T](name: String)
   
  // where we have a couple of alternatives,
  implicit val stringNamer = Namer[String]("string")
  implicit val intNamer = Namer[Int]("int")
   
  // we define the default one for a particular method like this:
  def myMethod[T](implicit default: T DefaultsTo String, namer: Namer[T]) = namer.name
   
  // Let's try it out in the REPL:
  // scala> myMethod
  // res0: String = string
   
  // scala> myMethod[Int]
  // res1: String = "int"

}