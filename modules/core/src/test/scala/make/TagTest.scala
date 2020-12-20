package make

import munit.FunSuite
import cats.effect.IO
import make.Tag.TpeTag

class TagTest extends FunSuite {

  // test("tags") {
  //   assertEquals(Tag.TpeTag[Int].tpe.render, "scala.Int")
  //   assertEquals(Tag.TpeTag[List[String]].tpe.render, "scala.collection.immutable.List[java.lang.String]")
  //   assertEquals(func[IO].tpe.render, "make.TagTest.ABC[cats.effect.IO]")
  //   assertEquals(func2[IO].tpe.render, "cats.effect.IO[scala.Int]")
  //   assertEquals(func3[String].tpe.render, "scala.collection.immutable.List[java.lang.String]")
  //   assertEquals(func[cats.Id].tpe.render, "make.TagTest.ABC[cats.Id]")
  //   assertEquals(func2[cats.Id].tpe.render, "cats.Id[scala.Int]")
  //   assertEquals(func4[String, IO[List[Int]]].tpe.render, "scala.Tuple2[java.lang.String, cats.effect.IO[scala.collection.immutable.List[scala.Int]]]")
  // }

  // class ABC[F[_]]
  // def func[F[_]: Tag.TCTag]: Tag.TpeTag[ABC[F]] = Tag.TpeTag[ABC[F]]

  // def func2[G[_]: Tag.TCTag]: Tag.TpeTag[G[Int]] = Tag.TpeTag[G[Int]]
  // def func3[A: Tag.TPTag]: Tag.TpeTag[List[A]] = Tag.TpeTag[List[A]]
  // def func4[A, B](implicit tag: TpeTag[(A, B)]): Tag.TpeTag[(A, B)] = Tag.TpeTag[(A, B)]

}