package make.zio

import zio.ZManaged
import make.Make
import make.Conflicts

object implicits {

  implicit val zmanagedEff: Make.EffError[({type F[A]= ZManaged[Any, Throwable, A]})#F] = {
    new Make.EffError[({type F[A]= ZManaged[Any, Throwable, A]})#F] {
      type F[A] = ZManaged[Any, Throwable, A]

      def map[A, B](fa: F[A])(f: A => B): F[B] = fa.map(f) 
      def pure[A](a: A): F[A] = ZManaged.effectTotal(a)
      def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = fa.flatMap(f)

      def raiseConflicts[A](conflicts: Conflicts): F[A] = {
        val err = new Exception(s"Conflits: ${conflicts.values.map(_.toString).mkString(",")}")
        ZManaged.fail(err)
      }
    }
  }
}