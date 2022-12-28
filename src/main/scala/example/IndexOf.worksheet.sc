import shapeless._
import ops.coproduct.IndexOf

// IndexOfはshapeless 2.4.0 以降でしか使えないことに注意
type ISB = Int :+: String :+: Boolean :+: CNil

def getIdx0[C <: Coproduct, A](implicit idx: IndexOf[C, A]): Nat = {
  idx()
}

// 型レベルの自然数なので、Nat型につぶすと内容が見えなくなる
val n1: Nat = getIdx0[ISB, Int]
val n2: Nat = getIdx0[ISB, String]
val n3: Nat = getIdx0[ISB, Boolean]

// 型パラメータを使ってNatを自由にする
// よくあるミスなので入出力の型を自由にしておくことを忘れないようにする
def getIdx[C <: Coproduct, A](implicit idx: IndexOf[C, A]): idx.Out = {
  idx()
}

val m1 = getIdx[ISB, Int]
val m2 = getIdx[ISB, String]
val m3 = getIdx[ISB, Boolean]

def check[N <: Nat](n: N, m: Int)(implicit ev: ops.nat.ToInt[N]): Boolean = {
  ev() == m
}

check(m1, 0)
check(m2, 1)
check(m3, 2)
