package make.internal

import cats.effect.Resource
import cats.Applicative

sealed trait Node[F[_], A] {
  def tag: NodeTag[A]
}

object Node {
  final case class Value[F[_], A](v: Resource[F, A], tag: NodeTag[A]) extends Node[F, A]
  final case class Func[F[_], In, A](prev: Node[F, In], f: In => Resource[F, A], tag: NodeTag[A]) extends Node[F, A]

  final case class Ap[F[_], In, A](prev: Node[F, In], f: Node[F, In => A], tag: NodeTag[A]) extends Node[F, A]

  def pure[F[_]: Applicative, A : NodeTag](v: A): Node[F, A] = Value(Resource.pure[F, A](v), NodeTag.of[A])
  def eff[F[_]: Applicative, A : NodeTag](v: F[A]): Node[F, A] = Value(Resource.liftF[F, A](v), NodeTag.of[A])
  def resource[F[_], A: NodeTag](v: Resource[F, A]): Node[F, A] = Value(v, NodeTag.of[A])
  def map[F[_]: Applicative, A, B: NodeTag](na: Node[F, A])(f: A => B): Node[F, B] =  Func(na, (a: A) => Resource.pure(f(a)), NodeTag.of[B])
  def mapF[F[_]: Applicative, A, B: NodeTag](na: Node[F, A])(f: A => F[B]): Node[F, B] =  Func(na, (a: A) => Resource.liftF(f(a)), NodeTag.of[B])
  def mapResource[F[_]: Applicative, A, B: NodeTag](na: Node[F, A])(f: A => Resource[F, B]): Node[F, B] =  Func(na, (a: A) => f(a), NodeTag.of[B])
  def ap[F[_], A, B: NodeTag](na: Node[F, A])(f: Node[F, A => B]): Node[F, B] = Ap(na, f, NodeTag.of[B])
}