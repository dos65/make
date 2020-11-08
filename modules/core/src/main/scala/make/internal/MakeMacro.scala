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
    val makeTc = weakTypeOf[Make[F, _]].typeConstructor

    val open = c.openImplicits

    val isSelfLoop =
      if (open.size > 2) {
        val c = open(2)
        c.sym.isMacro && c.sym.isMethod && c.sym.name.decodedName.toString == "debugHook"
      } else {
        false
      }

    if (isSelfLoop) {
      c.abort(c.enclosingPosition, "skip")
    } else {
      val makeTpe = appliedType(makeTc, ftpe.tpe, atpe.tpe)

      val instance = 
        if (state.failedTpe.contains(makeTpe)) EmptyTree else c.inferImplicitValue(makeTpe)

      instance match {
        case EmptyTree =>
          state.failedTpe.add(makeTpe)
          val trace = resolutionTrace(open, makeTc)
          trace.path.headOption.foreach { v =>
            val symSt = state.reverseTraces.getOrElse(v.tpe, mutable.HashMap.empty)
            symSt.update(v.sym, atpe.tpe)
            state.reverseTraces.update(v.tpe, symSt)
          }

          c.abort(c.enclosingPosition, s"Instance resolution for ${atpe.tpe} failed")
        case smt =>
          c.abort(c.enclosingPosition, "Instance exists: skip")
      }
    }
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
        } else {
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
    reverseTraces: mutable.HashMap[Type, mutable.HashMap[c.Symbol, c.Type]]
  ): InstanceSt = {
    reverseTraces.get(targetType) match {
      case None => InstanceSt.NoInstances(targetType)
      case Some(paths) =>
        val traces = paths.map { case (sym, tpe) =>
          val depSt = extractInstanceSt(tpe, reverseTraces)
          InstanceSt.FailedTrace(sym, tpe, depSt)
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
            val next = sb.append(s"\n${appendIdent}Failed at ${trace.sym.fullName} becase of:")
            render(next, level + 1, trace.dependencySt)
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
      sym: Symbol,
      dependencyTpe: Type,
      dependencySt: InstanceSt
    )

    case class FailedTraces(tpe: Type, traces: List[FailedTrace]) extends InstanceSt
  }

  case class Part(tpe: c.Type, sym: Symbol)
  case class Trace(path: List[Part])

  class DebugSt(
    var debug: Boolean = false,
    val failedTpe: mutable.HashSet[Type] = mutable.HashSet.empty,
    val reverseTraces: mutable.HashMap[Type, mutable.HashMap[c.Symbol, c.Type]] =
      mutable.HashMap.empty
  )
}
