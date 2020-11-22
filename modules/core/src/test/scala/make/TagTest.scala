package make

import munit.FunSuite
import cats.effect.IO

class TagTest extends FunSuite {

  test("tags") {
    assertEquals(Tag.TpeTag[Int].tpe.render, "scala.Int")
    assertEquals(Tag.TpeTag[List[String]].tpe.render, "scala.collection.immutable.List[java.lang.String]")
    assertEquals(func[IO].tpe.render, "make.TagTest.ABC[cats.effect.IO]")
    assertEquals(func2[IO].tpe.render, "cats.effect.IO[scala.Int]")
  }

  class ABC[F[_]]
  def func[F[_]: Tag.TCTag]: Tag.TpeTag[ABC[F]] = Tag.TpeTag[ABC[F]]

  def func2[G[_]: Tag.TCTag]: Tag.TpeTag[G[Int]] = Tag.TpeTag[G[Int]]
}