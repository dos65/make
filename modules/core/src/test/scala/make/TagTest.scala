package make

import munit.FunSuite
import scala.reflect.runtime.universe.WeakTypeTag
import scala.reflect.runtime.universe.Type
import cats.effect.IO

class TagTest extends FunSuite {

  // test("sad".only) {
  //   val x = implicitly[Tag.TpeTag[List[String]]]
  //   println(x.tpe.isFullyResolved)

  //   println(func[IO].tpe.isFullyResolved)
  // }

  // class ABC[F[_]]
  // def func[F[_]]: Tag.TpeTag[ABC[F]] = Tag.TpeTag[ABC[F]]
}