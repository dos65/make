package make.cats.effect

import cats.ApplicativeError
import make.Make
import cats.effect.Resource
import cats.MonadError
import make.Conflicts

object implicits {

  implicit def resourceEff[F[_]](
    implicit AE: ApplicativeError[F, Throwable]
  ): Make.EffError[({type R[A] = Resource[F, A]})#R] =
    new Make.EffError[({type R[A] = Resource[F, A]})#R] {
      type R[A] = Resource[F, A]

      def map[A, B](fa: R[A])(f: A => B): R[B] = fa.map(f) 
      def pure[A](a: A): R[A] = Resource.pure(a)
      def flatMap[A, B](fa: R[A])(f: A => R[B]): R[B] = fa.flatMap(f)

      def raiseConflicts[A](conflicts: Conflicts): R[A] = {
        val err = new Exception(s"Conflits: ${conflicts.values.map(_.toString).mkString(",")}")
        Resource.liftF(AE.raiseError(err))
      }
    }

}
