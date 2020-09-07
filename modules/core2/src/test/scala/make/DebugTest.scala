package make

import munit.FunSuite
import cats.effect.IO

import make.syntax._
import make.enableDebug._

case class A(i: Int)
object A {
  implicit val aMake2: Make[IO, A] = Make.pure(A(53))
  //implicit def aMake2(implicit s: Make[IO, String]): Make[IO, A] = Make.pure(A(53))
}
case class B(a: A)
case class C(b: B)
case class D(c: C, a: A)
trait Gi {
  def d: D
}
case class G(d: D) extends Gi
object G {
  trait make {
    implicit def gMake(implicit dm: Make[IO, D]): Make[IO, G] = dm.map(G(_))
  }
  object make extends make
  trait makeLower extends make {
    implicit def giMake(implicit gMake: Make[IO, G]): Make[IO, Gi] = gMake.map(v => v)
  }
  object makeLower extends makeLower
}

trait hoho {
    implicit def gMake(implicit dm: Make[IO, D]): Make[IO, String] = null
}
object hoho extends hoho

//object xx extends hoho with G.make

case class F(g: Gi)

class DebugTest extends FunSuite {

  test("asdasd".only) {
    //implicit val iiMake: MakeDef[IO, Int] = MakeDef.pure(13)
    //implicit val s: String = "sadda"
    implicit def aMakeX(implicit im: Make[IO, Int]): Make[IO, A] = im.map(A(_))
    implicit def bMake(implicit am: Make[IO, A]): Make[IO, B] = am.map(B(_))
    implicit def cMake(implicit bm: Make[IO, B]): Make[IO, C] = bm.map(C(_))

    Make.debugOf[IO, C]
  }

  test("zzz") {

    import G.make._
    import hoho._

//     implicit def bMake(implicit am: Make[IO, A]): Make[IO, B] = am.map(B(_))
//     implicit def cMake(implicit bm: Make[IO, B]): Make[IO, C] = bm.mapF(b => IO.pure(C(b)))
//     implicit def dMake(implicit deps: Make[IO, (C, A)]): Make[IO, D] = deps.mapN(D)
// //    implicit def gMake(implicit dm: Make[IO, D]): Make[IO, G] = dm.map(G)
// //    implicit def giMake(implicit gMake: Make[IO, G]): Make[IO, Gi] = gMake.map(v => v)
//     implicit def fMake(implicit gi: Make[IO, Gi]): Make[IO, F] = gi.map(F)

//     Make.debugOf[IO, F]
  }
}