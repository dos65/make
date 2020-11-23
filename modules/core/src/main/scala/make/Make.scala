package make

import make.internal.MakeMacro
import make.internal.MakeOps
import cats.Applicative
import scala.reflect.runtime.universe.TypeTag

sealed abstract class Make[F[_], A] {
  def tag: Tag[A]
}

object Make extends MakeTupleInstances with ContraMakeInstances with LowPrioMake {

  def of[F[_], A](implicit m: Make[F, A]): Make[F, A] = m

  final private[make] case class Value[F[_], A](
    v: F[A],
    tag: Tag[A]
  ) extends Make[F, A]

  final private[make] case class Bind[F[_], In, A](
    prev: Make[F, In],
    f: In => F[A],
    tag: Tag[A]
  ) extends Make[F, A]

  final private[make] case class Ap[F[_], In, A](
    prev: Make[F, In],
    f: Make[F, In => A],
    tag: Tag[A]
  ) extends Make[F, A]

  def pure[F[_]: Applicative, A: Tag](a: A): Make[F, A] =
    Value(Applicative[F].pure(a), Tag.of[A])

  def eff[F[_], A: Tag](v: F[A]): Make[F, A] =
    Value(v, Tag.of[A])


  def widen[A, B <: A]: Contra[A, B] = new Contra[A, B](identity)
  def contramap[A, B](f: B => A): Contra[A, B] = new Contra[A, B](f)

  final class Contra[A, B](private[make] val f: B => A)
}

trait ContraMakeInstances {

  implicit def contraMakeInstance[F[_]: Applicative, A, B](implicit
    contra: Make.Contra[A, B],
    m: Make[F, B],
    tagB: Tag[A]
  ): Make[F, A] = MakeOps.map(m)(contra.f)
}

trait LowPrioMake {
  implicit def debugInstance[F[_], A](implicit x: Debug[Make[F, A]]): Make[F, A] = x.v
}
