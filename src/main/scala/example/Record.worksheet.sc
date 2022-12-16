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

// HList example

type RGBA = Int :: Int :: Int :: Int :: HNil
val red: RGBA = 255 :: 0 :: 0 :: 255 :: HNil
val green: RGBA = 0 :: 255 :: 0 :: 255 :: HNil

def brendAverage0(x: RGBA, y: RGBA): RGBA = {
  val r = (x.at(0) + y.at(0)) / 2
  val g = (x.at(1) + y.at(1)) / 2
  val b = (x.at(2) + y.at(2)) / 2
  val a = (x.at(3) + y.at(3)) / 2
  return r :: g :: b :: a :: HNil
}
def brendAverage(xs: Int :: HList, ys: Int :: HList): Int :: HList = (xs, ys) match {
  case (x :: (x2: Int) :: HNil, y :: (y2: Int) :: HNil) => (x + y) / 2 :: (x2 + y2) / 2 :: HNil
  case (x :: (x2: Int) :: xs, y :: (y2: Int) :: ys) => (x + y) / 2 :: brendAverage(x2 :: xs, y2 :: ys)
  case otherwise => ???
}

brendAverage(red, green)

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
// import shapeless.ops.record.{Selector, Updater}
// //フィールドを持っていることは、特定の操作が可能であることとして読み替える
// // HListはH <: HListの形で受け取る
// def zeroHoge[H <: HList](r: H)(implicit updater: Updater[H, FieldType["hoge", Int]]): updater.Out = {
//   val newHoge: FieldType["hoge", Int] = shapeless.labelled.field(0)
//   updater(r, newHoge)
// }

// println(zeroHoge(hashFoo))

// いくつかのフィールドがあればよいような場合
import shapeless.ops.record.Extractor
// val rgba = ("r" ->> 255) :: ("g" ->> 128) :: ("b" ->> 64) :: ("a" ->> 255) :: HNil
// type RGB = 
//def fadeToBlack[H <: HList](rgb: H)(implicit extractor: Extractor[H, RGB]): H = ???

val rec = Record(foo = 42, bar = "hoge")
rec(Symbol("foo"))
rec(Symbol("bar"))


// howOld0(me) compile error

def getInt[H <: HList](h: H)(implicit sel: shapeless.ops.hlist.Selector[H, Int]) = {
  s"$h has int member ${sel(h)}"
}

getInt("foo" :: 42 :: HNil)


import shapeless.ops.hlist.Align

def reOrder[H <: HList](h: H)(implicit ali: Align[H, Int :: String:: HNil]) = {
  ali(h)
}

reOrder("hoge" :: 42 :: HNil)

import shapeless.ops.hlist.Diff
def removeInt[H <: HList](h: H)(implicit diff: Diff[H, Int :: Boolean :: HNil]) = {
  diff(h)
}

removeInt(42 :: "foo" :: true :: 666 :: HNil)

import shapeless.ops.hlist.Prepend
def prependInt[H <: HList](h: H)(implicit pre: Prepend[Int :: HNil, H]) = {
  pre(42 :: HNil, h)
}

prependInt("foo" :: "bar" :: HNil)

import shapeless.ops.record.Extractor
def getAge[H <: HList](rec: H)(implicit ex: Extractor[H, Record.`'age -> Int, 'name -> String`.T]) = {
  val extracted = ex(rec)
  val name = extracted(Symbol("name"))
  val age = extracted(Symbol("age"))
  s"$name is $age year(s) old"
}

val me = Record(name = "Windymelt", age = 29, gender = "male")
val usa = Record(name = "USA", capital = "Washington D.C.", age = 246)

def howOld0(person: Record.`'age -> Int`.T): Int = {
  person(Symbol("age"))
}

getAge(me)
getAge(usa)

case class Person(name: String, age: Int, gender: String)
val me1 = Person("Windymelt", 29, "male")
shapeless.Generic[Person].to(me1)
shapeless.Generic[Person].from("Windymelt" :: 29 :: "male" :: HNil)

case class Id(x: Long)
case class Car(id: Id, mass: Int, cost: Int)
case class Tweet(id: Id, user: String, content: String)

def selectId[A, H <: HList](x: A)(implicit gen: shapeless.Generic.Aux[A, H], sel: shapeless.ops.hlist.Selector[H, Id]): Id = {
  val hlist = gen.to(x)
  sel(hlist)
}

selectId(Car(Id(123), 1000, 3000000))
selectId(Tweet(Id(666), "@windymelt", "#welovescala"))

shapeless.LabelledGeneric[Person].to(me1)(Symbol("name"))

import scala.annotation.implicitNotFound
def greeting[A, H <: HList](x: A)(
  implicit
  @implicitNotFound("HListに変換できません")
  gen: shapeless.LabelledGeneric.Aux[A, H],
  @implicitNotFound("name: Stringを持つオブジェクトである必要があります")
  ext: shapeless.ops.record.Extractor[H, Record.`'name -> String`.T]
): String = {
  val extracted = ext(gen.to(x))
  val name = extracted(Symbol("name"))
  s"Hi, $name !"
}

val zundamon = Person("Zundamon", 1, "???")
case class Cat(name: String, age: Int)
val tama = Cat("Tama", 3)
greeting(zundamon)
greeting(tama)
