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

class MakeTest extends FunSuite {

  // @autoMake
  // class Anno(@anno.Sample a: Int) 
  // test("annotated") {

  //   import make.annotated._
  //   implicit val a = Make.pure[IO, Int :@: anno.Sample](42.annotated[anno.Sample])
  //   Make.of[IO, Anno]
  // }

  // test("asdsa".only) {

  //   case class A[F[_]](value: F[String])
  //   case class B[F[_]](a: A[F])
  //   case class C[F[_]](a: A[F])
  //   case class D[F[_], G[_]](b: B[F], c: C[G])

  //   implicit def aMakeOption[F[_]: Applicative]: Make[F, A[Option]] = Make.pure(A(Some("42")))
  //   implicit def aMakeIO[F[_]: Applicative]: Make[F, A[IO]] = Make.pure(A(IO.pure("42")))

  //   implicit def bMake[F[_]: Applicative, G[_]](
  //     implicit a: Make[F, A[G]]
  //   ): Make[F, B[G]] = 
  //     a.map(a => B(a))

  //   implicit def cMake[F[_]: Applicative, G[_]](
  //     implicit a: Make[F, A[G]]
  //   ): Make[F, C[G]] = 
  //     a.map(a => C(a))

  //   implicit def dMake[F[_]: Applicative, G[_], H[_]](
  //     implicit deps: Make[F, (B[G], C[H])]
  //   ): Make[F, D[G, H]] =
  //     deps.mapN(D(_, _))

  //   import enableDebug._
  //   val resolved = Make.debugOf[IO, D[Option, IO]]

  //   println(RenderTree.fromMake(resolved).render(0))

  //   pprint.pprintln(resolved)
  //   //println(resolved)
  //   //resolved.toGraph.x
  // }

  // test("asdsa2") {

  //   case class A[F[_]](value: F[String])
  //   case class B[F[_]](a: A[F])
  //   case class C[F[_]](a: A[F])
  //   case class D[F[_], G[_]](b: C[F], c: C[G])

  //   implicit def aMakeOption[F[_]: Applicative]: Make[F, A[Option]] = Make.pure(A(Some("42")))
  //   implicit def aMakeIO[F[_]: Applicative]: Make[F, A[IO]] = Make.pure(A(IO.pure("42")))

  //   implicit def bMake[F[_]: Applicative, G[_]](
  //     implicit a: Make[F, A[G]]
  //   ): Make[F, B[G]] = 
  //     a.map(a => B(a))

  //   implicit def cMake[F[_]: Applicative, G[_]](
  //     implicit a: Make[F, A[G]]
  //   ): Make[F, C[G]] = 
  //     a.map(a => C(a))

  //   implicit def dMake[F[_]: Applicative, G[_], H[_]](
  //     implicit deps: Make[F, (C[G], C[H])]
  //   ): Make[F, D[G, H]] =
  //     deps.mapN(D(_, _))

  //   import enableDebug._

  //   val resolved = Make.debugOf[IO, D[Option, IO]]

  //   println(RenderTree.fromMake(resolved).render(0))
  // }

  // test("asdsa3".only) {


  //   case class A[F[_]](value: F[String])
  //   case class C[F[_]](a: A[F])
  //   case class D[F[_], G[_]](b: C[F], c: C[G])

  //   implicit def aMake[F[_]: Applicative, Z[_]: Applicative]: Make[F, A[Z]] = Make.pure(A(Applicative[Z].pure("42")))

  //   implicit def cMake[F[_]: Applicative, G[_]: Applicative](
  //     implicit a: Make[F, A[G]]
  //   ): Make[F, C[G]] = 
  //     a.map(a => C(a))

  //   implicit def dMake[F[_]: Applicative, G[_]: Applicative, H[_]: Applicative](
  //     implicit deps: Make[F, (C[G], C[H])]
  //   ): Make[F, D[G, H]] =
  //     deps.mapN(D(_, _))

  //   // import enableDebug._

  //   val resolved = Make.of[IO, D[Option, IO]]

  //   println(RenderTree.fromMake(resolved).render(0))
  // }


  sealed trait RenderTree { self =>
    def render(n: Int): String = {
      val tabs = " " * n
      self match {
        case RenderTree.Value(tag) => s"${tabs}value#$tag"
        case RenderTree.Bind(prev, tag) => s"${tabs}apply#$tag(\n${prev.render(n+2)}\n$tabs)"
        case RenderTree.Ap(prev, f, tag) =>
          s"${tabs}apply#$tag(\n${f.render(n+2)}(\n${prev.render(n+4)}\n)\n$tabs)"
      }
    }
  }
  object RenderTree {

    case class Value(tag: String) extends RenderTree
    case class Bind(prev: RenderTree, tag: String) extends RenderTree
    case class Ap(prev: RenderTree, f: RenderTree, tag: String) extends RenderTree

    def fromMake[F[_], A](v: Make[F, A]): RenderTree = {
      v match {
        case make.Make.Value(v, tag) => RenderTree.Value(tag.typeTag.tpe.toString() + s"#//(${tag.sourcePos.path})")
        case make.Make.Bind(prev, f, tag) =>  RenderTree.Bind(fromMake(prev), tag.typeTag.tpe.toString() + s"#//(${tag.sourcePos.path})")
        case make.Make.Ap(prev, f, tag) =>
          RenderTree.Ap(fromMake(prev), fromMake(f), tag.typeTag.tpe.toString() + s"#//(${tag.sourcePos.path})") 
      }
    }
  }

  // test("instantiate once") {

  //   import make.syntax._

  //   case class Y[F[_]](value: String) 
  //   case class Z[F[_]](value: String) 

  //   case class X[G[_]](z: Z[G], y: Y[G])
  //   case class W[F[_]](y: Y[F], x: X[F])

  //   @autoMake
  //   case class Foo(w: W[IO], z: Z[IO])

  //   val rnd = new Random()

  //   implicit def yMake[F[_]: Applicative]: Make[F, Y[F]] =
  //     Make.pure(Y(rnd.nextString(20)))

  //   implicit def zMake[F[_]: Applicative]: Make[F, Z[F]] =
  //     Make.pure(Z(rnd.nextString(20)))

  //   implicit def xMake[G[_]: Applicative](implicit deps: Make[G, (Z[G], Y[G])]): Make[G, X[G]] =
  //     deps.mapN((z, y) => X(z, y))

  //   implicit def wMake[F[_]: Applicative](implicit deps: Make[F, (Y[F], X[F])]): Make[F, W[F]] =
  //     deps.mapN((y, x) => W(y, x))

  //   val resolve = Make.of[IO, (W[IO], X[IO], Foo)]

  //   println(resolve.toGraph.x.values.map(e => s"${e.id}\n\t${e.dependsOn}}").mkString("\n\n"))

  //   val (w, x, foo) = resolve.make.unsafeRunSync()

  //   assert(w.x == x)
  //   assert(foo.w == w)
  //   assert(foo.z == w.x.z)
  // }

  // test("asdsa".only) {
  //   println(implicitly[TpeTag[List[String]]])
  // }
}
