package make

import cats.Applicative
import cats.effect.Resource
import scala.reflect.api.TypeTags
import make.MakeDef.OkValue
import make.MakeDef.OkBind
import make.MakeDef.OkAp
import make.MakeDef.FullyFailed
import make.MakeDef.FailedBind
import make.internal.MakeMacro

sealed trait MakeDef[F[_], A] { self =>
  // TODO
  def map[B: Tag](f: A => B)(implicit F: Applicative[F]): MakeDef[F, B] = 
    self match {
      case fail: MakeDef.Failed[F, A] =>
        MakeDef.FailedBind(self, Tag.of[B])
      case ok: MakeDef.Resolved[F, A] =>
        MakeDef.OkBind(ok, (a: A) => Resource.pure(f(a)), Tag.of[B])
    }
}

trait LowPrioMakeDef {
  implicit def debugInstance[F[_], A](implicit x: Debug[MakeDef[F, A]]): MakeDef[F, A] = x.v
}

object MakeDef extends LowPrioMakeDef {

  sealed trait Failed[F[_], A] extends MakeDef[F, A]
  sealed trait Resolved[F[_], A] extends MakeDef[F, A]

  final private[make] case class OkValue[F[_], A](
    v: Resource[F, A],
    tag: Tag[A]
  ) extends Resolved[F, A]

  final private[make] case class OkBind[F[_], In, A](
    prev: Resolved[F, In],
    f: In => Resource[F, A],
    tag: Tag[A]
  ) extends Resolved[F, A]

  final private[make] case class OkAp[F[_], In, A](
    prev: Resolved[F, In],
    tag: Tag[A]
  ) extends Resolved[F, A]

  final private[make] case class FullyFailed[F[_], A]() extends Failed[F, A]

  final private[make] case class FailedBind[F[_], In, A](
    prev: MakeDef[F, In],
    tag: Tag[A]
  ) extends Failed[F, A]

  def pure[F[_]: Applicative, A: Tag](a: A): MakeDef[F, A] =
    OkValue(Resource.pure(a), Tag.of[A])

 }