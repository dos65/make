### Make

`Make` is an idiomatic version of the thing that is known as `dependency injection` but expressed in typeclass form
that makes it usage close to usual Scala code and provides compile-time checks.

### Usage

`build.sbt`:
```scala
libraryDependencies += "io.github.dos65" %% "make" % "0.0.2"
```

### Concept

Let's say that there is the following typeclass which describes how to instantiate `F[A]` where:
- `F[_]` is an initialization effect
- `A` a target type

```scala
trait Make[F[_], A] {
  def make: Either[Conflicts, F[A]]
}
```

Then we can define an instances of this tc for user-defined types.
```scala
import make._
import make.syntax._
import cats.effect._
import cats.effect.implicits._

case class Foo(v: Int, s: String)
object Foo {
  // just a const instance
  implicit val make: Make[IO, Foo] = Make.pure(Foo(42, "foo"))
}

// summon it and use somewhere else:
val fooMake: Make[IO, Foo] = Make.of[IO, Foo]
val f = for {
  foo <- IO.fromEither(fooMake.make)
  _ <-  // use foo
} yield ()
```

Let's make the example above a more interesting and do a real injection:
```scala
case class Foo(v: Int, s: String)
object Foo {
  // ask `Int` and `String` as a paramters and construct `Foo`
  implicit def make(implicit deps: Make[IO, (Int, String)]): Make[IO, Foo] =
    deps.mapN(Foo)
}
// now to summon it it's required to define `Make` for `Int` and `String`:
implicit val intMake: Make[IO, Int] = Make.pure(42)
implicit val strMake: Make[IO, String] = Make.pure("foo")

val fooMake: Make[IO, Foo] = Make.of[IO, Foo]
val f = for {
  foo <- IO.fromEither(fooMake.make)
  _ <-  // use foo
} yield ()
```