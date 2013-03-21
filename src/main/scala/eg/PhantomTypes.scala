package eg

object PhantomTypes {

  sealed trait State
  trait UnspecifiedBeans extends State
  trait SpecifiedBeans extends State

  case class BurritoBuilder[S <: State] private (beans: Option[String]) {

    def withBeans(s: String)(implicit ev: S =:= UnspecifiedBeans) =
      BurritoBuilder[SpecifiedBeans](beans = Some(s))

    def build(implicit ev: S =:= SpecifiedBeans) = "here's your burrito"
      
  }

  object BurritoBuilder {
    def empty = BurritoBuilder[UnspecifiedBeans](None)
  }

//BurritoBuilder.empty.build // won't compile, no beans
  BurritoBuilder.empty.withBeans("pinto").build // ok
//BurritoBuilder.empty.withBeans("pinto").withBeans("black") // won't compile, ambiguous beans

}