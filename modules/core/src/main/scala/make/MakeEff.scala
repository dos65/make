package make

trait MakeEff[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

object MakeEff {
  def apply[F[_]](implicit eff: MakeEff[F]): MakeEff[F] = eff
}