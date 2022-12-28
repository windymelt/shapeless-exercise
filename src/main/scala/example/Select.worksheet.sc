import shapeless._
import ops.coproduct.Selector

def select[I, C <: Coproduct](c: C)(implicit sel: Selector[C, I]): Option[I] = {
  sel(c)
}

type ISB = Int :+: String :+: Boolean :+: CNil
val isb1: ISB = Inl(42)
val isb2: ISB = Inr(Inr(Inl(false)))

val i: Option[Int] = select[Int, ISB](isb1)
val b: Option[Boolean] = select[Boolean, ISB](isb2)
