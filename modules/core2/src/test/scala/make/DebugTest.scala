package make

import munit.FunSuite
import cats.effect.IO

import make.enableDebug._

case class A(i: Int)
object A {
  //implicit val aMake2: MakeDef[IO, A] = MakeDef.pure(A(53))
}
case class B(a: A)
case class C(b: B)

class DebugTest extends FunSuite {

  test("asdasd".only) {
    // implicit val x = MakeDef.pure[IO, Int](42)
    // implicit val x = implicitly[Make.Def].pure[IO, Int](42)

    //implicit val iiMake: MakeDef[IO, Int] = MakeDef.pure(13)
    implicit def aMakeX(implicit im: MakeDef[IO, Int]): MakeDef[IO, A] = im.map(A(_))
    implicit def bMake(implicit am: MakeDef[IO, A]): MakeDef[IO, B] = am.map(B(_))
    implicit def cMake(implicit bm: MakeDef[IO, B]): MakeDef[IO, C] = bm.map(C(_))

    // implicit def aMake(implicit dep: MakeDef[IO, Int]): MakeDef[IO, A] =
    //   dep.map(i => A(2))

//    implicit val x: Int = 53  
    // Make.debugDerive[IO, B] 
    // implicitly[Exported[MakeDef[IO, A]]]
    MakeDef.debugOf[IO, C]
    //println(implicitly[MakeDef[IO, C]])
    // println(implicitly[MakeDef[IO, B]])

  }
}