package make

import make.internal.MakeOps
import make.MakeEff

object syntax extends MakeTupleSyntax {
  implicit def makeToBasicSyntax[F[_], A](make: Make[F, A]): MakeBasicSyntax[F, A] =
    new MakeBasicSyntax(make)
}

final class MakeBasicSyntax[F[_], A](private val m: Make[F, A]) extends AnyVal {
  def map[B: Tag](f: A => B)(implicit F: MakeEff[F]): Make[F, B] =
    MakeOps.map(m)(f)

  def mapF[B: Tag](f: A => F[B])(implicit F: MakeEff[F]): Make[F, B] =
    MakeOps.mapF(m)(f)

  def ap[B: Tag](mf: Make[F, A => B]): Make[F, B] =
    MakeOps.ap(m)(mf)

  def toGraph(implicit F: MakeEff[F]): Either[Conflicts, Graph[F, A]] =
    Graph.fromMake(m)

  // def toEff(implicit F: EffError[F]): F[A] = {
  //   toDag match {
  //     case Left(conflicts) => F.raiseConflicts(conflicts)
  //     case Right(dag) => dag.toEff
  //   }
  // }
}