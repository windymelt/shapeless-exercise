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

hlis match {
  case (i: Int) :: (s: String) :: HNil => s"int: $i, string: $s"
}

def stringifyIntString(is: IS): String = {
  val i = is.head
  val s = is.tail.head
  s"int: $i, string: $s"
}

import shapeless.HList
def unknownHList(hl: HList): Int = {
  hl.productArity
}
unknownHList(hlis)

// 基礎概念: Coproduct: 「どちらかの型」を表現する型
// EitherをList風に扱いやすくしたものと考える
import shapeless.{:+:, CNil, Inl, Inr}
type IntOrString = Int :+: String :+: CNil

case class ErrorA(msg: String)
case class ErrorB(msg: String)
case class ErrorC(msg: String)
case class ErrorD(msg: String)
type Errors = ErrorA :+: ErrorB :+: ErrorC :+: ErrorD :+: CNil
val ea: Errors = Inl(ErrorA("error a"))

import shapeless.syntax.inject._
val ec: Errors = ErrorC("injecting").inject[Errors]
val iosx: IntOrString = 42.inject[IntOrString]

import shapeless.ops.coproduct.Inject // for converter object
val s3: IntOrString = Inject[IntOrString, String].apply("foo")

// inl / inr
// Coproductの要素にアクセスするには、Inl / Inrという仕組みを使う
// InlとInrの名前はInjection(単射)に由来している
// 型L | RからはLとRとを取り出す単射がそれぞれ存在することから
import shapeless.{Inl, Inr}
val ios1: IntOrString = Inl(42)
val ios2: IntOrString = Inr(Inl("foo"))

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
