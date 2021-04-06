package make

import munit.FunSuite
import cats.effect.IO
import make.Tag.SourcePos

import cats.implicits._
import make.syntax._
import cats.Applicative
import scala.util.Random
import make.Tag.TpeTag
import make.syntax._
import scala.concurrent.ExecutionContext
import make.internal.MakeOps
import shapeless.test.illTyped

class MakeTest extends FunSuite {

  // test("annotated") {
  //   @autoMake
  //   class Anno(@anno.Sample a: Int) 

  //   import make.annotated._
  //   implicit val a = Make.pure[IO, Int :@: anno.Sample](42.annotated[anno.Sample])
  //   Make.of[IO, Anno]
  // }

  // test("zero parameters autoMake") {
  //   @autoMake
  //   class ZeroArg {
  //     def hello: String = "hello"
  //   }
  //   val out = Make.of[IO, ZeroArg].make.unsafeRunSync.hello 
  //   assertEquals(out, "hello")
  // }

  // test("instantiate once") {

  //   case class Y[F[_]](value: String) 
  //   case class Z[F[_]](value: String) 

  //   case class X[G[_]](z: Z[G], y: Y[G])
  //   case class W[F[_]](y: Y[F], x: X[F])

  //   @autoMake
  //   case class Foo(w: W[IO], z: Z[IO])

  //   val rnd = new Random()

  //   implicit def yMake[F[_]: Applicative: Tag.TCTag]: Make[F, Y[F]] =
  //     Make.pure(Y(rnd.nextString(20)))

  //   implicit def zMake[F[_]: Applicative: Tag.TCTag]: Make[F, Z[F]] =
  //     Make.pure(Z(rnd.nextString(20)))

  //   implicit def xMake[G[_]: Applicative: Tag.TCTag](implicit deps: Make[G, (Z[G], Y[G])]): Make[G, X[G]] =
  //     deps.mapN((z, y) => X(z, y))

  //   implicit def wMake[F[_]: Applicative: Tag.TCTag](implicit deps: Make[F, (Y[F], X[F])]): Make[F, W[F]] =
  //     deps.mapN((y, x) => W(y, x))

  //   val resolve = Make.of[IO, (W[IO], X[IO], Foo)]

  //   val (w, x, foo) = resolve.make.unsafeRunSync()

  //   assert(w.x == x)
  //   assert(foo.w == w)
  //   assert(foo.z == w.x.z)
  // }

  // test("higher kind tags 1") {

  //   case class A[F[_]](value: F[String])
  //   case class C[F[_]](a: A[F])
  //   case class D[F[_], G[_]](b: C[F], c: C[G])

  //   implicit def aMake[F[_]: Applicative, Z[_]: Applicative: Tag.TCTag]: Make[F, A[Z]] = Make.pure(A(Applicative[Z].pure("42")))

  //   implicit def cMake[F[_]: Applicative, G[_]: Applicative: Tag.TCTag](
  //     implicit a: Make[F, A[G]]
  //   ): Make[F, C[G]] = 
  //     a.map(a => C(a))

  //   implicit def dMake[F[_]: Applicative, G[_]: Applicative: Tag.TCTag, H[_]: Applicative: Tag.TCTag](
  //     implicit deps: Make[F, (C[G], C[H])]
  //   ): Make[F, D[G, H]] =
  //     deps.mapN(D(_, _))

  //   import enableDebug._

  //   val resolved = Make.debugOf[IO, D[Option, IO]]
  //   val d = resolved.make.unsafeRunSync()
  //   assertEquals(d.b, C(A("42".some)))
  //   assertEquals(d.c.a.value.unsafeRunSync(), "42")
  // }

  // test("higher kind tags 2") {

  //   case class A[F[_]](value: F[String])
  //   case class B(a: String)

  //   implicit def aMake[F[_]: Applicative, Z[_]: Applicative: Tag.TCTag]: Make[F, A[Z]] = Make.pure(A(Applicative[Z].pure("42")))

  //   implicit def bMake[F[_]: Applicative: Tag.TCTag](
  //     implicit a: Make[F, A[F]]
  //   ): Make[F, B] = 
  //     a.mapF(a => a.value.map(v => B(v)))


  //   // import enableDebug._

  //   val resolved = Make.of[IO, B]
  //   assertEquals(resolved.make.unsafeRunSync(), B("42"))
  // }

  // test("diverging implicit(tuples)") {

  //   case class A[F[_]](value: F[String])
  //   case class B(a: String, i: Int)

  //   @autoMake
  //   case class C(b: B)

  //   implicit def cMake[F[_]: Applicative](
  //     implicit dep: Dep[F, B]
  //   ): Make[F, C] = dep.value.map(C(_))

  //   implicit def bMake[F[_]: Applicative](
  //     implicit
  //        deps: Dep[F, (Int, A[F])],
  //   ): Make[F, B] = 
  //     deps.value.mapFN((i, a) => a.value.map(v => B(v, i)))

  //   import enableDebug._

  //   implicit val aMake:Make[IO, A[IO]] = Make.pure[IO, A[IO]](A(IO("42")))
  //   implicit val intMake = Make.pure[IO, Int](42)

    
  //   val resolved = Make.debugOf[IO, (C, B)]
  //   assertEquals(resolved.make.unsafeRunSync(), (C(B("42", 42)), B("42", 42)))
  // }

  // test("doesn't allow cycles") {
  //   illTyped(
  //     """
  //       @autoMake
  //       case class A(b: B)
  //       @autoMake
  //       case class B(a: A)
  //       Make.of[IO, B]
  //     """
  //   )
  // }

  test("sample debug") {
    case class A(i: Int)
    object A {
      implicit def aMake[F[_]:Applicative](implicit iMake: Make[F, Int]):Make [F, A] = iMake.map(A(_))
      //implicit def aMake(implicit iMake: Make[IO, Int]):Make[IO, A] = iMake.map(A(_))
    }
    @autoMake
    case class B(a: A)
    implicit val intMake: Make[IO, Int] = Make.pure(1)

    val make = Make.of[IO, B]
    println(make.make.unsafeRunSync)
  }

}
