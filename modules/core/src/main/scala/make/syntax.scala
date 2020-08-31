package make

import make.Make.DepsCtx
import make.internal.NodeTag

object syntax {

  implicit class DepsCtxSingle[F[_], A, B](val ctx: DepsCtx[F, A]) extends AnyVal {
    def funcN[Out: NodeTag](f: A => Out): Make[F, Out] =
      ctx.func(f)
  }
  implicit class DepsCtxTuple2[F[_], A, B](val ctx: DepsCtx[F, (A, B)]) extends AnyVal {
    def funcN[Out: NodeTag](f: (A, B) => Out): Make[F, Out] =
      ctx.func{case (a, b) => f(a, b)}
  }
  implicit class DepsCtxTuple3[F[_], A, B, C](val ctx: DepsCtx[F, (A, B, C)]) extends AnyVal {
    def funcN[Out: NodeTag](f: (A, B, C) => Out): Make[F, Out] =
      ctx.func{case (a, b, c) => f(a, b, c)}
  }
  implicit class DepsCtxTuple4[F[_], A, B, C, D](val ctx: DepsCtx[F, (A, B, C, D)]) extends AnyVal {
    def funcN[Out: NodeTag](f: (A, B, C, D) => Out): Make[F, Out] =
      ctx.func{case (a, b, c, d) => f(a, b, c, d)}
  }
  implicit class DepsCtxTuple5[F[_], A, B, C, D, E](val ctx: DepsCtx[F, (A, B, C, D, E)]) extends AnyVal {
    def funcN[Out: NodeTag](f: (A, B, C, D, E) => Out): Make[F, Out] =
      ctx.func{case (a, b, c, d, e) => f(a, b, c, d, e)}
  }

  implicit class DepsCtxTuple6[Eff[_], A, B, C, D, E, F](val ctx: DepsCtx[Eff, (A, B, C, D, E, F)]) extends AnyVal {
    def funcN[Out: NodeTag](ff: (A, B, C, D, E, F) => Out): Make[Eff, Out] =
      ctx.func{case (a, b, c, d, e, f) => ff(a, b, c, d, e, f)}
  }
}