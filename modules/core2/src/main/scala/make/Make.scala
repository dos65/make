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

}

