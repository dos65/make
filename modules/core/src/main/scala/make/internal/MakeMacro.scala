package make.internal

import scala.reflect.macros.whitebox
import make.Make
import make.Debug
import scala.collection.mutable

class MakeMacro(val ctx: whitebox.Context) extends DebugStateMacro(ctx) {

  import c.universe._

  def debug[F[_], A](implicit
    ftpe: WeakTypeTag[F[X] forSome { type X }],
    atpe: WeakTypeTag[A]
  ): c.Expr[Make[F, A]] = {

    val makeTc = weakTypeOf[Make[F, _]].typeConstructor
    val searchType = appliedType(makeTc, ftpe.tpe, atpe.tpe)

    debugState.debug = true
    val out = c.inferImplicitValue(searchType)
    debugState.debug = false
    out match {
      case EmptyTree =>
        val st = extractInstanceSt(atpe.tpe, debugState.reverseTraces)
        val message = renderInstanceSt(st)
        c.abort(c.enclosingPosition, s"Make for ${atpe.tpe} not found\n" + message)
      case tree =>
        val message = s"Debug: OK!\n\tMake instance for ${atpe.tpe} exists.\n\nRemove debug usage."
        c.info(c.enclosingPosition, message, true)
        c.Expr[Make[F, A]](tree)
    }
  }

  def debugHook[F[_], A](implicit
    ftpe: WeakTypeTag[F[X] forSome { type X }],
    atpe: WeakTypeTag[A]
  ): c.Expr[Debug[Make[F, A]]] = {

    if (!debugState.debug) c.abort(c.enclosingPosition, "debug is not enabled")
    debugState.resolveCache.get(atpe.tpe) match {
      case Some(ResolveSt.InProgress) => c.abort(c.enclosingPosition, "skip")
      case Some(ResolveSt.Resolved(tree)) => c.abort(c.enclosingPosition, "skip")
      case None =>
        val makeTc = weakTypeOf[Make[F, _]].typeConstructor
        val makeTpe = appliedType(makeTc, ftpe.tpe, atpe.tpe)
        debugState.stack.append(atpe.tpe)
        val pos = debugState.stack.size
        debugState.resolveCache.update(atpe.tpe, ResolveSt.InProgress)
        val tree = c.inferImplicitValue(makeTpe)
        debugState.resolveCache.update(atpe.tpe, ResolveSt.Resolved(tree))
        debugState.stack.remove(pos - 1)
        tree match {
          case EmptyTree =>
            debugState.stack.lastOption.foreach(debugState.registerFailedReason(atpe.tpe, _))
        }
        c.abort(c.enclosingPosition, "skip")
      case _ => c.abort(c.enclosingPosition, "skip")
    } 
  }

  private def extractInstanceSt(
    targetType: c.Type,
    reverseTraces: mutable.HashMap[Type, List[Type]]
  ): InstanceSt = {
    reverseTraces.get(targetType) match {
      case None => InstanceSt.NoInstances(targetType)
      case Some(paths) =>
        val traces = paths.map { tpe =>
          val depSt = extractInstanceSt(tpe, reverseTraces)
          InstanceSt.FailedTrace(tpe, depSt)
        }
        InstanceSt.FailedTraces(targetType, traces.toList)
    }
  }

  private def renderInstanceSt(st: InstanceSt): String = {

    def render(sb: StringBuilder, level: Int, st: InstanceSt): StringBuilder = {
      val ident = "  " * level
      val appendIdent = ident + " "
      sb.append(s"\n${ident}${st.tpe}:")
      st match {
        case InstanceSt.NoInstances(_) =>
          sb.append(s"\n${appendIdent}${st.tpe} not found")
        case InstanceSt.FailedTraces(_, traces) =>
          traces.foldLeft(sb) { case (sb, trace) =>
            render(sb, level + 1, trace.dependencySt)
          }
      }
    }

    render(new StringBuilder, 1, st).toString
  }

  sealed trait InstanceSt { self =>
    def tpe: Type
    def lastTypes: List[Type] = {

      def extract(acc: List[Type], v: InstanceSt): List[Type] = {
        v match {
          case InstanceSt.NoInstances(tpe) => tpe :: acc
          case InstanceSt.FailedTraces(_, traces) =>
            traces.foldLeft(acc){ case (a, st) => extract(a, st.dependencySt)}
        }
      }
      extract(List.empty, self)
    }
  }
  object InstanceSt {
    case class NoInstances(tpe: Type) extends InstanceSt

    case class FailedTrace(
      dependencyTpe: Type,
      dependencySt: InstanceSt
    )

    case class FailedTraces(tpe: Type, traces: List[FailedTrace]) extends InstanceSt
  }

}
