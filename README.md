### Make

Make is a library that implements a kind of `dependency injection` in a functional manner using just one typeclass and inductive derivation.
Its features are __compile time checks__ and __clear error messages__.

### Getting started

`build.sbt`:
```scala
libraryDependencies += "io.github.dos65" %% "make" % "0.0.1"
```

Make is a typeclass with the following signature: `Make[F[_], A]` where
- `F[_]` is an initialization effect
- `A` a target type


```
import cats.effect.Resource
import cats.effect.IO

class Foo(v: Int)

object Foo {
  implicit val make: Make[Resource[IO, ?], Foo] = Make.pure(Foo(42))
}
```

