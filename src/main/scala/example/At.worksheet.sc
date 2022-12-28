import shapeless._
import ops.coproduct.At

type ISB = Int :+: String :+: Boolean :+: CNil
val isb1: ISB = Inl(42)
val isb2: ISB = Inr(Inl("foo"))

// 型レベルで表現された自然数。
// shapelessは型レベルでHList / Coproductを扱うため、インデックスアクセスなどの自然数が必要になる局面では自然数を型レベルに持ち上げる必要がある
val zero = nat._0
val one = nat(1)
val two = nat(2)

def at[C <: Coproduct, N <: Nat, O](c: C, idx: N)(implicit atInstance: At.Aux[C, N, O]): Option[O] = {
  atInstance(c)
}

at(isb1, zero)
at(isb1, one)
at(isb1, two)

at(isb2, zero)
at(isb2, one)
at(isb2, two)
