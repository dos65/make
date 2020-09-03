package make

import munit.FunSuite
import cats.effect.IO

import make.enableDebug._

case class A(i: Int)
object A {
  implicit val aMake2: MakeDef[IO, A] = MakeDef.pure(A(53))
  //implicit def aMake2(implicit s: MakeDef[IO, String]): MakeDef[IO, A] = MakeDef.pure(A(53))
}
case class B(a: A)
case class C(b: B)

class DebugTest extends FunSuite {

  test("asdasd".only) {
    //implicit val iiMake: MakeDef[IO, Int] = MakeDef.pure(13)
    //implicit val s: String = "sadda"
    implicit def aMakeX(implicit im: MakeDef[IO, Int]): MakeDef[IO, A] = im.map(A(_))
    implicit def bMake(implicit am: MakeDef[IO, A]): MakeDef[IO, B] = am.map(B(_))
    implicit def cMake(implicit bm: MakeDef[IO, B]): MakeDef[IO, C] = bm.map(C(_))

    MakeDef.debugOf[IO, C]
  }
}