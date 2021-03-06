### Make

`Make` is an idiomatic version of the thing that is known as `dependency injection` but expressed in typeclass form
that makes it usage close to usual Scala code and provides compile-time checks.

### Usage

`build.sbt`:
```scala
libraryDependencies += "io.github.dos65" %% "make" % "0.0.2"
```

Imports:
```scala
import make._
import make.syntax._
```

### Defining instances

Speaking roughly, `Make` might be treated as a typeclass with the following signature:
```scala
trait Make[F[_], A] {
  def make: F[A]
}
```

Where:
- `F[_]` is an initialization effect
- `A` a target type

Working with it isn't different from how we usually work with typeclasses.

Instance definition examples:
```scala
// define instance
case class Foo(a: Int)
object Foo {
  // just a pure instance
  implicit val make: Make[IO, Foo] = Make.pure(Foo(42))
  // or construct it in F[_]
  implicit val make: Make[IO, Foo] = Make.eff(IO(Foo(42)))
}

case class Bar(foo: Foo)
object Bar {
  // define instance that is is build from dependency
  implicit def make(implicit dep: Make[IO, Foo]): Make[IO, Bar] =
    dep.map(foo => Bar(foo))
}

case class Baz(foo: Foo, bar: Bar)
object Baz {
  // use tuple for several depencies
  implicit def make(implicit dep: Make[IO, (Foo, Bar)]): Make[IO, Baz] =
    dep.mapN((foo, bar) => Baz(foo, bar))
} 

// or use @autoMake annotation to generate the code above
@autoMake
class AutoBaz(foo: Foo, bar: Bar)
```

Then summon instances that you need and use:
```scala
// single Foo
val fooMake: Make[IO, Foo] = Make.of[IO, Foo]
// several instances
val several: Make[IO, (Baz, AutoBaz)] = Make.of[IO, (Baz, AutoBaz)]

// use 
import make.syntax._

val fooIO: IO[Foo] = fooMake.make
```

### Debug

In case if instance can't be infered it isn't clear what went wrong.
To get a detailed information `debugOf`:
```scala
val bazMake = Make.of[IO, Baz]
// [error] FromReadme.scala:39:26: could not find implicit value for parameter m: make.Make[cats.effect.IO,example.FromReadme.Baz]
//    could not find implicit value for parameter m: make.Make[cats.effect.IO,example.FromReadme.Baz]
// [error]     val bazMake = Make.of[IO, Baz]
// [error]                          ^


// detailed error with debug
import make.enableDebug._
val bazMake = Make.debugOf[IO, Baz]
// [error] FromReadme.scala:40:31: Make for example.FromReadme.Baz not found
// [error]         Make instance for example.FromReadme.Baz:
// [error]                 Failed at example.FromReadme.Baz.make becase of:
// [error]                 Make instance for (example.FromReadme.Foo, example.FromReadme.Bar):
// [error]                         Failed at make.MakeTupleInstances.tuple2 becase of:
// [error]                         Make instance for example.FromReadme.Foo:
// [error]                                 Make instance for example.FromReadme.Foo not found
// [error]     val bazMake = Make.debugOf[IO, Baz]
// [error]                               ^
```

### Subtypes

`A` type in `Make[F, A]` is invariant.
So, to use interfaces in other instances you need to provide `ContraMake`:
```scala
trait Foo

@autoMake
class FooImpl(smt: Smth) extends Foo

implicit val fooFromFooImpl = ContraMake.widen[FooImpl, Foo]
```

### Choose the F[_]

It's up to you what `F[_]` to use. The only requirement on it is to have `cats.Monad` instance.
`Resourse[F, ?]` should cover all needs.
