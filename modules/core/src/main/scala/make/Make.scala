package make

import make.internal.Node
import make.internal.NodeTag
import cats.effect.Resource
import cats.Applicative
import make.internal.NodeTag.Pos
import scala.reflect.classTag
import scala.reflect.ClassTag
import scala.language.experimental.macros
import cats.Monad

trait Make[F[_], A] {
  def node: Node[A]
  final def asResource(implicit x: Monad[F]): Resource[F, A] = internal.Dag.toResource[F, A](node)
}

object Make {

  def context[F[_]: Applicative]: Make.Ctx[F] = new Ctx[F]

  def inCtx[F[_]: Make.Ctx]: DerivePartillyApplyed[F] = 
    new DerivePartillyApplyed[F]
  
  class DerivePartillyApplyed[F[_]] {
    def derive[A]: Make[F, A] = 
      macro make.internal.DepsCtxMacros.materializeMake[F, A]
  }

  final class Ctx[F[_]: Applicative] {
    def pure[A: NodeTag](v: A): Make[F, A] =
      createFromNodeUnsafe(Node.Pure(v, NodeTag.of[A]))
    
    def effect[A: NodeTag](v: F[A]): Make[F, A] =
      createFromNodeUnsafe(Node.Eff(Resource.liftF(v), NodeTag.of[A]))
    
    def resource[A: NodeTag](v: Resource[F, A]): Make[F, A] =
      createFromNodeUnsafe(Node.Eff(v, NodeTag.of[A]))
  }  

  final class DepsCtx[F[_]: Applicative, A](deps: Make[F, A]) {
    def pure[B: NodeTag](f: A => B): Make[F, B] =
      createFromNodeUnsafe(Node.Func(deps.node, f, NodeTag.of[B]))

    def effect[B: NodeTag](f: A => F[B]): Make[F, B] =
      createFromNodeUnsafe(Node.EffFunc(deps.node, (a: A) => Resource.liftF(f(a)), NodeTag.of[B]))

    def resource[B: NodeTag](f: A => Resource[F, B]): Make[F, B] =
      createFromNodeUnsafe(Node.EffFunc(deps.node, f, NodeTag.of[B]))
  }

  object DepsCtx {

    implicit def derive[F[_], A]: Make.DepsCtx[F, A] =
      macro make.internal.DepsCtxMacros.materializeDeps[F, A]
  }

  def createFromNodeUnsafe[F[_], A](v: Node[A]): Make[F, A] =
    new Make[F, A] {
      val node: Node[A] = v
    }
}