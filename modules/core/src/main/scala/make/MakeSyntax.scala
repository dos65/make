package make

import make.internal.MakeOps
import cats.Monad
import cats.Functor
import cats.Applicative

object syntax extends MakeTupleSyntax {
  implicit def makeToBasicSyntax[F[_], A](make: Make[F, A]): MakeBasicSyntax[F, A] =
    new MakeBasicSyntax(make)
}

final class MakeBasicSyntax[F[_], A](private val m: Make[F, A]) extends AnyVal {
  def map[B: Tag](f: A => B)(implicit F: Applicative[F]): Make[F, B] =
    MakeOps.map(m)(f)

  def mapF[B: Tag](f: A => F[B]): Make[F, B] =
    MakeOps.mapF(m)(f)

  def ap[B: Tag](mf: Make[F, A => B]): Make[F, B] =
    MakeOps.ap(m)(mf)

  def toGraph(implicit F: Monad[F]): Either[Conflicts, Graph[F, A]] =
    Graph.fromMake(m)

  def make(implicit F: Monad[F]): Either[Conflicts, F[A]] =
    toGraph.map(_.initEff) 
}
