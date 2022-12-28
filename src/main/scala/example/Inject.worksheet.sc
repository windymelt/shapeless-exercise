import shapeless._
import ops.coproduct.Inject

def inject[C <: Coproduct, I](i: I)(implicit inj: Inject[C, I]): C = {
  inj(i)
}

type ISB = Int :+: String :+: Boolean :+: CNil

val isb1: ISB = inject[ISB, String]("foo")
val isb2: ISB = inject[ISB, Boolean](false)
