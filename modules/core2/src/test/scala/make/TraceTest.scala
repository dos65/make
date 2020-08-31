package make

import munit.FunSuite
import cats.effect.IO
//import make.failed._

case class A(i: Int)
object A {
  //implicit val aMake2: MakeDef[IO, A] = MakeDef.pure(A(42))
}
case class B(a: A)
case class C(b: B)

class TraceTest extends FunSuite {

  test("asdasd".only) {
    // implicit val x = MakeDef.pure[IO, Int](42)
    // implicit val x = implicitly[Make.Def].pure[IO, Int](42)
    // implicit def aMake(implicit im: Make[IO, Int], md: Make.Def): Make[IO, A] = md.pure(A(42))
    // implicit def bMake(implicit am: Make[IO, A], md: Make.Def): Make[IO, B] = md.pure(B(A(42)))

    // import X._

    
    // implicit def aMake(implicit dep: MakeDef[IO, Int]): MakeDef[IO, A] =
    //   dep.map(i => A(2))

//    implicit val x: Int = 53  
//_root_.make.GenFailed.instance[make.GenFailed[cats.effect.IO,make.A]](_root_.make.MakeDef.FullyFailed[cats.effect.IO, make.A])
    println(implicitly[MakeDef[IO, A]])
    // println(implicitly[MakeDef[IO, B]])

  }
}