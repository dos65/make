package make.internal

import make.Make
import scala.reflect.{ClassTag, classTag}
import make.internal.NodeTag.Pos

object NodeOps {

  def product[A, B](a: Node[A], b: Node[B]): Node[(A, B)] = {
    Node.Ap(
      b,
      Node.Func(a, (a: A) => (b: B) => (a, b), NodeTag.of[B => (A, B)]),
      NodeTag.of[(A, B)]
    )
  }

  def product[A, B, C](a: Node[A], b: Node[B], c: Node[C]): Node[(A, B, C)] = {
    Node.Ap(
      c,
      Node.Ap(
        b,
        Node.Func(a, (a: A) => (b: B) => (c: C) => (a, b, c), NodeTag.of[B => C => (A, B, C)]),
        NodeTag.of[C => (A, B, C)]
      ) ,
      NodeTag.of[(A, B, C)]
    )
  }

  def product[A, B, C, D](a: Node[A], b: Node[B], c: Node[C], d: Node[D]): Node[(A, B, C, D)] = {
    Node.Ap(
      d,
      Node.Ap(
        c,
        Node.Ap(
          b,
          Node.Func(a, (a: A) => (b: B) => (c: C) => (d: D) => (a, b, c, d), NodeTag.of[B => C => D => (A, B, C, D)]),
          NodeTag.of[C => D => (A, B, C, D)]
        ) ,
        NodeTag.of[D => (A, B, C, D)]
      ),
      NodeTag.of[(A, B, C, D)]
    )
  }
}