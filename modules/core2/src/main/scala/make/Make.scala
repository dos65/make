package make

import scala.reflect.ClassTag
import scala.reflect.classTag

import cats.Applicative
import cats.effect.Resource

import make.internal.MakeMacro
import scala.annotation.implicitNotFound

sealed trait Make[F[_], A] {
  def tag: Tag[A]
  final def make: Resource[F, A] = ???
}

object Make {

  final private[make] case class Value[F[_], A](
    v: Resource[F, A],
    tag: Tag[A]
  ) extends Make[F, A]

  final private[make] case class Bind[F[_], In, A](
    prev: Make[F, In],
    f: In => F[Resource[F, A]],
    tag: Tag[A]
  ) extends Make[F, A]

  final private[make] case class Ap[F[_], In, A](
    prev: Make[F, In],
    f: Make[F, In => A],
    tag: Tag[A]
  ) extends Make[F, A]


  def debugDerive[F[_], A]: MakeDef[F, A] = 
    macro MakeMacro.debugDerive[F, A]

  final class Def {
    def pure[F[_]: Applicative, A: Tag](a: A): Make[F, A] = Value(Resource.pure(a), Tag.of[A])
  }
  object Def {
    // private[make] val instance = new Def
    // implicit def materialize: Def =
    //   macro MakeMacro.materializeDef
  }
}

