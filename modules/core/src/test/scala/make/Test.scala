package make

import munit.FunSuite
import cats.effect.IO

import syntax._
import scala.util.Random

class MakeTest extends FunSuite {

  test("sample") {

    case class A(v: String)
    case class B(i: Int)
    case class C(a: A, b: B)
    case class D(c: C, a: A)

    object module {
      implicit def aMake(implicit mk: Make.Ctx[IO]): Make[IO, A] = mk.pure(A("a"))
      implicit def bMake(implicit mk: Make.Ctx[IO]): Make[IO, B] = mk.pure(B(42))

      implicit def cMake(implicit mk: Make.DepsCtx[IO, (A, B)]): Make[IO, C] = mk.funcN(C)
      implicit def dMake(implicit mk: Make.DepsCtx[IO, (C, A)]): Make[IO, D] = mk.funcN(D)
    }

    object module2 {
      implicit def aMake(implicit mk: Make.Ctx[IO]): Make[IO, A] = mk.pure(A("a"))
      implicit def bMake(implicit mk: Make.Ctx[IO]): Make[IO, B] = mk.pure(B(42))

      implicit def cMake(implicit mk: Make.DepsCtx[IO, (A, B)]): Make[IO, C] = mk.funcN(C)
      implicit def dMake(implicit mk: Make.DepsCtx[IO, (C, A)]): Make[IO, D] = mk.funcN(D)
    }

    import module._

    implicit val ctx = Make.context[IO]
    val resource = Make.inCtx[IO].derive[D]
  }

  test("asdsad") {

    import scala.reflect.runtime.universe._

    val x = typeTag[List[Int]]
    val y = typeTag[List[String]]

    println(x)
    println(y)
    println(x == y)
  }


}