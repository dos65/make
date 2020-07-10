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
import java.util.concurrent.Future

trait Make[F[_], A] {
  def node: Node[F, A]
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
      createFromNodeUnsafe(Node.pure(v))
    
    def effect[A: NodeTag](v: F[A]): Make[F, A] =
      createFromNodeUnsafe(Node.eff(v))
    
    def resource[A: NodeTag](v: Resource[F, A]): Make[F, A] =
      createFromNodeUnsafe(Node.resource(v))
  }  

  final class DepsCtx[F[_]: Applicative, A](deps: Make[F, A]) {
    def func[B: NodeTag](f: A => B): Make[F, B] =
      createFromNodeUnsafe(Node.map(deps.node)(f))

    def funcF[B: NodeTag](f: A => F[B]): Make[F, B] =
      createFromNodeUnsafe(Node.mapF(deps.node)(f))

    def funcResource[B: NodeTag](f: A => Resource[F, B]): Make[F, B] =
      createFromNodeUnsafe(Node.mapResource(deps.node)(f))
  }

  object DepsCtx {

    implicit def derive[F[_], A]: Make.DepsCtx[F, A] =
      macro make.internal.DepsCtxMacros.materializeDeps[F, A]
  }

  def createFromNodeUnsafe[F[_], A](v: Node[F, A]): Make[F, A] =
    new Make[F, A] {
      val node: Node[F, A] = v
    }
}