package make.ce

import cats.effect.Resource
import cats.effect.IO
import make.MakeEff
import cats.Applicative

object resource {

  type IOResource[A] = Resource[IO, A]

  implicit def ceResourceEff[F[_]: Applicative]: MakeEff[Resource[F, ?]] =
    new MakeEff[Resource[F, ?]] {
      def map[A, B](fa: Resource[F, A])(f: A => B): Resource[F, B] = fa.map(f)
      def pure[A](a: A): Resource[F, A] = Resource.pure(a)
      def flatMap[A, B](fa: Resource[F, A])(f: A => Resource[F, B]): Resource[F, B] = fa.flatMap(f)
    }
}
