package make

import scala.reflect.ClassTag
import scala.reflect.classTag

import cats.Applicative
import cats.effect.Resource

import make.internal.MakeMacro

sealed abstract class Make[F[_], A] { self =>
  def tag: Tag[A]
  final def make: Resource[F, A] = ???
}

object Make extends LowPrioMake {

  final private[make] case class Value[F[_], A](
    v: Resource[F, A],
    tag: Tag[A]
  ) extends Make[F, A]

  final private[make] case class Bind[F[_], In, A](
    prev: Make[F, In],
    f: In => Resource[F, A],
    tag: Tag[A]
  ) extends Make[F, A]

  final private[make] case class Ap[F[_], In, A](
    prev: Make[F, In],
    f: Make[F, In => A],
    tag: Tag[A]
  ) extends Make[F, A]

  def pure[F[_]: Applicative, A: Tag](a: A): Make[F, A] =
    Value(Resource.pure(a), Tag.of[A])
}

trait LowPrioMake {
  implicit def debugInstance[F[_], A](implicit x: Debug[Make[F, A]]): Make[F, A] = x.v
}