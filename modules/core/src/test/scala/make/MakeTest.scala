package make

import munit.FunSuite
import cats.effect.IO
import make.Tag.SourcePos

import make.syntax._

class MakeTest extends FunSuite {

  import ioEff._

  test("basic") {
    val a = Make.pure[IO, Int](1)
    assertEquals(a.tag.sourcePos, SourcePos("make.MakeTest.a", 12, 31))
  }

  test("map") {
    val a = Make.pure(42)
    val b = a.map(_.toString)
    assertEquals(b.tag.sourcePos, SourcePos("make.MakeTest.b", 18, 18))
  }

  test("conflict") {
    val a = Make.pure[IO, Int](42)
    val b = a.map(identity)
    assert(a.toDag.isRight)
    assert(b.toDag.isLeft)
  }
  
}