package make.internal

import make.Tag
import make.Make
import cats.Applicative
import cats.effect.Resource
import make.Make.Eff

trait MakeBasicOps {

  def map[F[_]: Eff, A, B: Tag](ma: Make[F, A])(f: A => B): Make[F, B] =
    Make.Bind(
      ma,
      (a: A) => Eff[F].pure(f(a)),
      Tag.of[B]
    )

  def mapF[F[_]: Eff, A, B: Tag](ma: Make[F, A])(f: A => F[B]): Make[F, B] =
    Make.Bind(
      ma,
      (a: A) =>  f(a),
      Tag.of[B]
    )

  // def mapResource[F[_], A, B: Tag](ma: Make[F, A])(f: A => Resource[F, B]): Make[F, B] =
  //   Make.Bind(
  //     ma,
  //     f,
  //     Tag.of[B]
  //   )

  def ap[F[_], A, B: Tag](ma: Make[F, A])(mf: Make[F, A => B]): Make[F, B] =
    Make.Ap(
      ma,
      mf,
      Tag.of[B]
    )

}