package make.internal

import make.Make
import scala.reflect.{ClassTag, classTag}
import make.internal.NodeTag.Pos
import cats.Applicative

object NodeOps {

  def product[F[_]: Applicative, A, B](a: Node[F, A], b: Node[F, B]): Node[F, (A, B)] = {
    Node.ap(b)(Node.map(a)(a => (b: B) => (a, b)))
  }

  def product[F[_]: Applicative, A, B, C](a: Node[F, A], b: Node[F, B], c: Node[F, C]): Node[F, (A, B, C)] = {
    Node.ap(c)(
      Node.ap(b)(
        Node.map(a)(a => b => c => (a, b, c))
      )
    )
  }

  def product[F[_]: Applicative, A, B, C, D](a: Node[F, A], b: Node[F, B], c: Node[F, C], d: Node[F, D]): Node[F, (A, B, C, D)] = {
    Node.ap(d)(
      Node.ap(c)(
        Node.ap(b)(
          Node.map(a)(a => b => c => d => (a, b, c, d))
        )
      )
    )
  }

  def product[F[_]: Applicative, A, B, C, D, E](a: Node[F, A], b: Node[F, B], c: Node[F, C], d: Node[F, D], e: Node[F, E]): Node[F, (A, B, C, D, E)] = {
    Node.ap(e)(
      Node.ap(d)(
        Node.ap(c)(
          Node.ap(b)(
            Node.map(a)(a => b => c => d => e => (a, b, c, d, e))
          )
        )
      )
    )
  }

}