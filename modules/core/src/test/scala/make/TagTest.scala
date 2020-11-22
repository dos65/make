package make

import munit.FunSuite
import scala.reflect.runtime.universe.WeakTypeTag
import scala.reflect.runtime.universe.Type
import cats.effect.IO

class TagTest extends FunSuite {

  test("tags") {
    assertEquals(Tag.TpeTag2[Int].tpe.render, "scala.Int")
    assertEquals(Tag.TpeTag2[List[String]].tpe.render, "scala.collection.immutable.List[java.lang.String]")
    assertEquals(func[IO].tpe.render, "make.TagTest.ABC[cats.effect.IO]")
    assertEquals(func2[IO].tpe.render, "cats.effect.IO[scala.Int]")
  }

  class ABC[F[_]]
  def func[F[_]: Tag.TCTag]: Tag.TpeTag2[ABC[F]] = Tag.TpeTag2[ABC[F]]

  def func2[G[_]: Tag.TCTag]: Tag.TpeTag2[G[Int]] = Tag.TpeTag2[G[Int]]
}