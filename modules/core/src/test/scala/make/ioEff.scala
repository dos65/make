package make

import cats.effect.IO

object ioEff {

  implicit def ioEff: Make.Eff[IO] =
    new Make.Eff[IO] {
      def map[A, B](fa: IO[A])(f: A => B): IO[B] = fa.map(f)
      def pure[A](a: A): IO[A] = IO.pure(a)
      def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
    }
}