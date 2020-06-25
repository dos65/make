package make.internal

import cats.effect.Resource

sealed trait Node[A] {
  def tag: NodeTag[A]
}
sealed trait EffNode[F[_], A] extends Node[A]

object Node {
  final case class Pure[A](v: A, tag: NodeTag[A]) extends Node[A]
  final case class Func[In, A](prev: Node[In], f: In => A, tag: NodeTag[A]) extends Node[A]

  final case class Eff[F[_], A](v: Resource[F, A], tag: NodeTag[A]) extends EffNode[F, A]
  final case class EffFunc[F[_], In, A](prev: Node[In], f: In => Resource[F, A], tag: NodeTag[A]) extends EffNode[F, A]

  final case class Ap[In, A](prev: Node[In], f: Node[In => A], tag: NodeTag[A]) extends Node[A]
}