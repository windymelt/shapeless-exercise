import shapeless._
import syntax.singleton._ // for ->>
import record._ // for .get

val r = ("foo" ->> 42) :: ("bar" ->> 666) :: ("buzz" ->> 100) :: HNil

// getメソッドがSelectorを要求する
r.get("foo")

import shapeless.ops.record.Selector
def getFooBar[R <: HList](r: R)(
  implicit sf: Selector[R, "foo"],
  sb: Selector[R, "bar"],
): String = {
  val foo = r.get("foo")
  val bar = r.get("bar")
  s"$foo, $bar"
}

getFooBar(r)

