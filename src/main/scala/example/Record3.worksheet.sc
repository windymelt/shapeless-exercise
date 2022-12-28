import shapeless._
import syntax.singleton._ // for ->>
import ops.record.SelectAll

val r = ("foo" ->> 42) :: ("bar" ->> 666) :: ("buzz" ->> 100) :: HNil

def getFooBar[R <: HList](r: R)(
  implicit saf: SelectAll[R, "foo" :: "bar" :: HNil]
): String = {
  // pattern matchできて便利
  val f :: b :: HNil = saf(r)
  s"$f, $b"
}

getFooBar(r)
