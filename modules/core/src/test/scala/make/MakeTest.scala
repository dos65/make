package make

import munit.FunSuite
import cats.effect.IO
import make.Tag.SourcePos

import make.syntax._
import cats.Applicative
import scala.util.Random

class MakeTest extends FunSuite {

  test("annotated") {
    @autoMake
    class Anno(@anno.Sample a: Int) 

    import make.annotated._
    implicit val a = Make.pure[IO, Int :@: anno.Sample](42.annotated[anno.Sample])
    Make.of[IO, Anno]
  }

  test("instantiate once") {

    import make.syntax._

    case class Y[F[_]](value: String) 
    case class Z[F[_]](value: String) 

    case class X[G[_]](z: Z[G], y: Y[G])
    case class W[F[_]](y: Y[F], x: X[F])

    @autoMake
    case class Foo(w: W[IO], z: Z[IO])

    val rnd = new Random()

    implicit def yMake[F[_]: Applicative]: Make[F, Y[F]] =
      Make.pure(Y(rnd.nextString(20)))

    implicit def zMake[F[_]: Applicative]: Make[F, Z[F]] =
      Make.pure(Z(rnd.nextString(20)))

    implicit def xMake[G[_]: Applicative](implicit deps: Make[G, (Z[G], Y[G])]): Make[G, X[G]] =
      deps.mapN((z, y) => X(z, y))

    implicit def wMake[F[_]: Applicative](implicit deps: Make[F, (Y[F], X[F])]): Make[F, W[F]] =
      deps.mapN((y, x) => W(y, x))

    val resolve = Make.of[IO, (W[IO], X[IO], Foo)]

    val (w, x, foo) = resolve.make.unsafeRunSync()

    assert(w.x == x)
    assert(foo.w == w)
    assert(foo.z == w.x.z)
  }
}
