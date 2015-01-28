package eg

object PhantomTypes {

  sealed trait BeanState
  trait UnspecifiedBeans extends BeanState
  trait SpecifiedBeans extends BeanState

  sealed trait CheeseState
  trait UnspecifiedCheese extends CheeseState
  trait SpecifiedCheese extends CheeseState

  case class Burrito(beans: String, cheese: String)

  class BurritoBuilder[B <: BeanState, C <: CheeseState] private (beans: Option[String], cheese: Option[String]) {

    def withBeans(s: String)(implicit ev: B =:= UnspecifiedBeans) =
      new BurritoBuilder[SpecifiedBeans, C](Some(s), cheese)

    def withCheese(s: String)(implicit ev: C =:= UnspecifiedCheese) =
      new BurritoBuilder[B, SpecifiedCheese](beans, Some((s)))

    def build(implicit ev1: B =:= SpecifiedBeans, ev2: C =:= SpecifiedCheese) = 
      Burrito(beans.get, cheese.get) // hm
      
  }

  object BurritoBuilder {
    def empty = new BurritoBuilder[UnspecifiedBeans, UnspecifiedCheese](None, None)
  }

//BurritoBuilder.empty.build // won't compile, no beans
  BurritoBuilder.empty.withBeans("pinto").withCheese("fresco").build // ok
//BurritoBuilder.empty.withBeans("pinto").withBeans("black") // won't compile, ambiguous beans

}