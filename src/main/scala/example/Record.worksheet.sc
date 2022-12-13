import shapeless.ops.record.Extractor

// Shapeless基礎文法最速マスター

// libraryDependencies += "com.chuusai" % "shapeless_2.13" % "2.3.5"

// 覚えておきたいShapeless基礎知識: 4つのパッケージ
// shapeless, shapeless.ops, shapeless.syntax, shapeless.record

// Shapelessの何がエエの
// - PerlやRubyなどを使っている人向けの説明: ハッシュに型が付くぜ!!すごいだろ!!
// - Scala使ってる人向けの説明: 動的にフィールドを生やしたり減らしたりできるクラスを型安全に表現できるぜ!!

// 基礎概念
// HList: Heterogenerous List: Listと違って、異なる型を混載できるリスト

// HList literal
import shapeless.HNil
val hlis = 42 :: "foo" :: HNil
// HList type literal
import shapeless.:: // 値を作るときの::はメソッドなので区別すること
type IS = Int :: String :: HNil

// 基礎概念: Coproduct: 「どちらかの型」を表現する型
// EitherをList風に扱いやすくしたものと考える
import shapeless.{:+:, CNil}
type IntOrString = Int :+: String :+: CNil

// inl / inr
// Coproductの要素にアクセスするには、Inl / Inrという仕組みを使う
// InlとInrの名前はInjection(単射)に由来している
// 型L | RからはLとRとを取り出す単射がそれぞれ存在することから
import shapeless.{Inl, Inr}
val ios1: IntOrString = Inl(42)

// 最小限のRecordを作るには2つのimportが必要
import shapeless.HNil
import shapeless.syntax.singleton._

// "hash" contains "foo" field
val hashFoo = ("foo" ->> "bar") :: ("hoge" ->> 42) :: HNil
hashFoo

// toListでListに潰せる
import shapeless.ops.hlist.ToList
hashFoo.toList

// 一般のMap-likeな操作
// by-index access
hashFoo(0)
hashFoo.updateAtWith(0)(_ => "buzz")

// by-label access
import shapeless.record._
hashFoo("foo")
hashFoo + ("third" ->> 3)
hashFoo - "foo"
// hashFoo - "bar" // does not compile!

// 特定のフィールドを持つRecordのみ受け付けるようなメソッド
import shapeless.HList
import shapeless.labelled.FieldType
import shapeless.ops.record.{Selector, Updater}
//フィールドを持っていることは、特定の操作が可能であることとして読み替える
// HListはH <: HListの形で受け取る
def zeroHoge[H <: HList](r: H)(implicit updater: Updater[H, FieldType["hoge", Int]]): updater.Out = {
  val newHoge: FieldType["hoge", Int] = shapeless.labelled.field(0)
  updater(r, newHoge)
}

println(zeroHoge(hashFoo))

// いくつかのフィールドがあればよいような場合
import shapeless.ops.record.Extractor
// val rgba = ("r" ->> 255) :: ("g" ->> 128) :: ("b" ->> 64) :: ("a" ->> 255) :: HNil
// type RGB = 
//def fadeToBlack[H <: HList](rgb: H)(implicit extractor: Extractor[H, RGB]): H = ???
