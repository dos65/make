package make

import cats.effect.Resource
import cats.Applicative

object syntax {
  implicit def makeToApOps[F[_], A](make: Make[F, A]): MakeApOps[F, A] =
    new MakeApOps(make)
}

final class MakeApOps[F[_], A](private val m: Make[F, A]) extends AnyVal {
  def map[B: Tag](f: A => B)(implicit F: Applicative[F]): Make[F, B] =
    Make.Bind(
      m,
      (a: A) => Resource.pure(f(a)),
      Tag.of[B]
    )
  
  def ap[B: Tag](mf: Make[F, A => B]): Make[F, B] =
    Make.Ap(
      m,
      mf,
      Tag.of[B]
    )
}