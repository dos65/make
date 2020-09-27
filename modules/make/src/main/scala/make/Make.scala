package make

import scala.reflect.ClassTag
import scala.reflect.classTag

import cats.Applicative
import cats.effect.Resource

import make.internal.MakeMacro
import make.internal.MakeOps

sealed abstract class Make[F[_], A] { self =>
  def tag: Tag[A]
}

object Make extends MakeTupleInstances with LowPrioMake {

  def of[F[_], A](implicit m: Make[F, A]): Make[F, A] = m

  final private[make] case class Value[F[_], A](
    v: F[A],
    tag: Tag[A]
  ) extends Make[F, A]

  final private[make] case class Bind[F[_], In, A](
    prev: Make[F, In],
    f: In => F[A],
    tag: Tag[A]
  ) extends Make[F, A]

  final private[make] case class Ap[F[_], In, A](
    prev: Make[F, In],
    f: Make[F, In => A],
    tag: Tag[A]
  ) extends Make[F, A]

  def pure[F[_]: Eff, A: Tag](a: A): Make[F, A] =
    Value(Eff[F].pure(a), Tag.of[A])

  def value[F[_]: Eff, A: Tag](v: F[A]): Make[F, A] =
    Value(v, Tag.of[A])

  // def pure[F[_]: Applicative, A: Tag](a: A): Make[F, A] =
  //   Value(Resource.pure(a), Tag.of[A])

  // def liftF[F[_]: Applicative, A: Tag](fa: F[A]): Make[F, A] =
  //   Value(Resource.liftF(fa), Tag.of[A])

  // def resource[F[_], A: Tag](v: Resource[F, A]): Make[F, A] =
  //   Value(v, Tag.of[A])


  trait Eff[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def pure[A](a: A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }
  object Eff {
    def apply[F[_]](implicit eff: Eff[F]): Eff[F] = eff
  }

  implicit def contraMakeInstance[F[_]: Eff, B, A](
    implicit contra: ContraMake[B, A], m: Make[F, B], tag: Tag[A]
  ): Make[F, A] = MakeOps.map(m)(contra.f)
}

final class ContraMake[B, A](private[make] val f: B => A)
object ContraMake {
  def widen[B, A >: B]: ContraMake[B, A] = new ContraMake[B, A](identity)
  def apply[B, A](f: B => A): ContraMake[B, A] = new ContraMake[B, A](f)
}


trait LowPrioMake {
  implicit def debugInstance[F[_], A](implicit x: Debug[Make[F, A]]): Make[F, A] = x.v
}