package make

import cats.effect.Resource
import cats.Applicative
import make.internal.MakeOps

object syntax extends MakeTupleSyntax {
  implicit def makeToBasicSyntax[F[_], A](make: Make[F, A]): MakeBasicSyntax[F, A] =
    new MakeBasicSyntax(make)
}

final class MakeBasicSyntax[F[_], A](private val m: Make[F, A]) extends AnyVal {
  def map[B: Tag](f: A => B)(implicit F: Applicative[F]): Make[F, B] =
    MakeOps.map(m)(f)

  def mapF[B: Tag](f: A => F[B])(implicit F: Applicative[F]): Make[F, B] =
    MakeOps.mapF(m)(f)

  def mapResource[B: Tag](f: A => Resource[F, B]): Make[F, B] =
    MakeOps.mapResource(m)(f)
  
  def ap[B: Tag](mf: Make[F, A => B]): Make[F, B] =
    MakeOps.ap(m)(mf)

}