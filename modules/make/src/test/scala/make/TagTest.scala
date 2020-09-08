package make

import munit.FunSuite
import make.Tag.SourcePos

class TagTest extends FunSuite {

  test("base") {
    val x = implicitly[Tag[List[Int]]]
    val y = implicitly[Tag[List[String]]]
    println(x)
    println(y)
  }
}