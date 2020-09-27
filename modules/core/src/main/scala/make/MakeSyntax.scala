package make

import make.internal.MakeOps
import make.Make.Eff
import make.Make.EffError

object syntax extends MakeTupleSyntax {
  implicit def makeToBasicSyntax[F[_], A](make: Make[F, A]): MakeBasicSyntax[F, A] =
    new MakeBasicSyntax(make)
}

final class MakeBasicSyntax[F[_], A](private val m: Make[F, A]) extends AnyVal {
  def map[B: Tag](f: A => B)(implicit F: Eff[F]): Make[F, B] =
    MakeOps.map(m)(f)

  def mapF[B: Tag](f: A => F[B])(implicit F: Eff[F]): Make[F, B] =
    MakeOps.mapF(m)(f)

  def ap[B: Tag](mf: Make[F, A => B]): Make[F, B] =
    MakeOps.ap(m)(mf)

  def toDag(implicit F: Eff[F]): Either[Conflicts, Dag[F, A]] =
    Dag.fromMake(m)

  def toEff(implicit F: EffError[F]): F[A] = {
    toDag match {
      case Left(conflicts) =>
        // // TODO
        // val err = new Exception(s"Conflits: ${conflicts.map(_.toString).mkString_(",")}")
        F.raiseConflicts(conflicts)
      case Right(dag) =>
        dag.toEff
    }
  }
}