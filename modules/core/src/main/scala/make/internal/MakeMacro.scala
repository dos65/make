package make.internal

import scala.reflect.macros.whitebox
import make.Make
import make.Debug
import scala.collection.mutable

class MakeMacro(val c: whitebox.Context) {

  import c.universe._

  val state = MacroState.getOrElseUpdate[DebugSt](c.universe, new DebugSt)

  val debugInstanceFullName = "make.LowPrioMake.debugInstance"
  val debugHookFullName = "make.enableDebug.debugHook"

  val shapelessLazyTc = weakTypeOf[shapeless.Lazy[_]].typeConstructor

  def debug[F[_], A](implicit
    ftpe: WeakTypeTag[F[X] forSome { type X }],
    atpe: WeakTypeTag[A]
  ): c.Expr[Make[F, A]] = {

    val makeTc = weakTypeOf[Make[F, _]].typeConstructor
    val searchType = appliedType(makeTc, ftpe.tpe, atpe.tpe)

    state.debug = true
    val out = c.inferImplicitValue(searchType)
    state.debug = false
    out match {
      case EmptyTree =>
        val st = extractInstanceSt(atpe.tpe, state.reverseTraces)
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

    if (!state.debug) c.abort(c.enclosingPosition, "debug is not enabled")
    state.resolveCache.get(atpe.tpe) match {
      case Some(ResolveSt.InProgress) => c.abort(c.enclosingPosition, "skip")
      case Some(ResolveSt.Resolved(tree)) => c.abort(c.enclosingPosition, "skip")
      case None =>
        val makeTc = weakTypeOf[Make[F, _]].typeConstructor
        val makeTpe = appliedType(makeTc, ftpe.tpe, atpe.tpe)
        state.stack.append(atpe.tpe)
        val pos = state.stack.size
        state.resolveCache.update(atpe.tpe, ResolveSt.InProgress)
        val tree = c.inferImplicitValue(makeTpe)
        state.resolveCache.update(atpe.tpe, ResolveSt.Resolved(tree))
        state.stack.remove(pos - 1)
        tree match {
          case EmptyTree =>
            println(s"FAILED: ${atpe.tpe} ${state.stack}")
            state.stack.lastOption.foreach(state.registerFailedReason(atpe.tpe, _))
        }
        c.abort(c.enclosingPosition, "skip")
      case _ => c.abort(c.enclosingPosition, "skip")
    } 

    // val open = c.openImplicits

    // val isSelfLoop =
    //   if (open.size > 2) {
    //     val c = open(2)
    //     c.sym.isMacro && c.sym.isMethod && c.sym.name.decodedName.toString == "debugHook"
    //   } else {
    //     false
    //   }

    // if (isSelfLoop) {
    //   c.abort(c.enclosingPosition, "skip")
    // } else { 

    //   val makeTc = weakTypeOf[Make[F, _]].typeConstructor
    //   val makeTpe = appliedType(makeTc, ftpe.tpe, atpe.tpe)

    //   val instance = {
    //      state.resolveCache.get(atpe.tpe) match {
    //        case Some(ResolveSt.InProgress) => c.abort(c.enclosingPosition, "skip")
    //        case Some(ResolveSt.Resolved(tree)) => tree
    //        case None =>
    //         state.stack.append(atpe.tpe)
    //         val pos = state.stack.size
    //         state.resolveCache.update(atpe.tpe, ResolveSt.InProgress)
    //         val tree = c.inferImplicitValue(makeTpe)
    //         state.resolveCache.update(atpe.tpe, ResolveSt.Resolved(tree))
    //         state.stack.remove(pos - 1)
    //         tree match {
    //           case EmptyTree =>
    //             println(s"FROMSTACK: ${atpe.tpe} : " + state.stack.mkString(", "))
    //         }
    //         tree
    //      } 
    //   } 


    //   instance match {
    //     case EmptyTree =>

    //       val trace = resolutionTrace(open, makeTc)
    //       trace.path.sliding(2).foreach { v =>
    //         val target = v(0)
    //         val reason = v(1)
    //         state.registerFailedReason(target.tpe, reason.sym, reason.tpe)
    //       }
    //       trace.path.headOption.foreach { v => 
    //         state.registerFailedReason(atpe.tpe, v.sym, v.tpe)
    //       }

    //       c.abort(c.enclosingPosition, s"Instance resolution for ${atpe.tpe} failed")
    //     case smt =>
    //       c.abort(c.enclosingPosition, "Instance exists: skip")
    //   }
    // }
  }

  private def resolutionTrace(
    openImplicits: List[c.ImplicitCandidate],
    makeTc: c.Type
  ): Trace = {
    val filtered = openImplicits
      .filter(c => !isDebugCandidate(c))
      .flatMap { c =>
        val dealiased = c.pt.dealias
        val tc = dealiased.typeConstructor
        if (tc =:= makeTc) {
          val tpe = dealiased.typeArgs(1)
          Some(Part(tpe, c.sym))
        }else {
          None
        }
      }
    Trace(filtered)
  }

  private def isDebugCandidate(candidate: c.ImplicitCandidate): Boolean = {
    val fullPath = candidate.sym.fullName
    fullPath == debugHookFullName || fullPath == debugInstanceFullName
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

  sealed trait InstanceSt {
    def tpe: Type
  }
  object InstanceSt {
    case class NoInstances(tpe: Type) extends InstanceSt

    case class FailedTrace(
      dependencyTpe: Type,
      dependencySt: InstanceSt
    )

    case class FailedTraces(tpe: Type, traces: List[FailedTrace]) extends InstanceSt
  }

  case class Part(tpe: c.Type, sym: Symbol)
  case class Trace(path: List[Part])

  sealed trait ResolveSt
  object ResolveSt {
    case object InProgress extends ResolveSt
    case class Resolved(tree: Tree) extends ResolveSt
  }

  class DebugSt(
    var debug: Boolean = false,
    val stack: mutable.ListBuffer[Type] = mutable.ListBuffer.empty,
    val resolveCache: mutable.HashMap[Type, ResolveSt] = mutable.HashMap.empty,
    val reverseTraces: mutable.HashMap[Type, List[Type]] =
      mutable.HashMap.empty
  ) {

    def registerFailedReason(targetTpe: Type, prev: Type): Unit = {
      val curr = reverseTraces.getOrElse(prev, List.empty)
      val next = targetTpe :: curr
      reverseTraces.update(prev, next)
    }

  }
}
