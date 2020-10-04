package make.internal

import make.Tag
import make.Make
import make.MakeEff

trait MakeBasicOps {

  def map[F[_]: MakeEff, A, B: Tag](ma: Make[F, A])(f: A => B): Make[F, B] =
    Make.Bind(
      ma,
      (a: A) => MakeEff[F].pure(f(a)),
      Tag.of[B]
    )

  def mapF[F[_]: MakeEff, A, B: Tag](ma: Make[F, A])(f: A => F[B]): Make[F, B] =
    Make.Bind(
      ma,
      (a: A) => f(a),
      Tag.of[B]
    )

  def ap[F[_], A, B: Tag](ma: Make[F, A])(mf: Make[F, A => B]): Make[F, B] =
    Make.Ap(
      ma,
      mf,
      Tag.of[B]
    )

}
