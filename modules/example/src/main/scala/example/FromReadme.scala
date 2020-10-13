package example

import make._
import make.syntax._
import cats._
import cats.implicits._
import cats.effect._

object FromReadme extends IOApp {

  // // define instance
  // case class Foo(a: Int)
  // object Foo {
  //   // just a pure instance
  //   //implicit val make: Make[IO, Foo] = Make.pure(Foo(42))
  //   // or construct it in F[_]
  //   //implicit val make: Make[IO, Foo] = Make.eff(IO(Foo(42)))
  // }

  // case class Bar(foo: Foo)
  // object Bar {
  //   // define instance that is is build from dependency
  //   implicit def make(implicit dep: Make[IO, Foo]): Make[IO, Bar] =
  //     dep.map(foo => Bar(foo))
  // }

  // case class Baz(foo: Foo, bar: Bar)
  // object Baz {
  //   // use tuple for several depencies
  //   implicit def make(implicit dep: Make[IO, (Foo, Bar)]): Make[IO, Baz] =
  //     dep.mapN((foo, bar) => Baz(foo, bar))
  // } 

  // // or use @autoMake annotation to generate the code above
  // @autoMake
  // class AutoBaz(foo: Foo, bar: Bar)
  case class Foo(i: Int)
  object Foo {
    implicit val make: Make[IO, Foo] = Make.pure[IO, Int](1).map(i => Foo(i))
  }

  @autoMake
  case class Bar(i: Int)

  implicit val intInstance: Make[IO, Int] = Make.pure(42)

// // in this case `v` will be `Left(Conflicts)`
// // because `Foo.make` adds an additional `Int` into resolution graph
// val v: Either[Conflicts, IO[A]] = Make.of[IO, Bar].make

  override def run(args: List[String]): IO[ExitCode] = {
    val v = Make.of[IO, (Bar, Foo)].make
    println(v)
    ???
  }

}