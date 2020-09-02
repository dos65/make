package make.internal

import scala.reflect.macros.whitebox
import make.Make
import make.MakeDef
import make.Debug
import scala.collection.mutable

class MakeMacro(val c: whitebox.Context) {

  import c.universe._


  val state = MacroState.getOrElseUpdate[DebugSt](c.universe, new DebugSt)

  def debugDerive[F[_], A](implicit ftpe: WeakTypeTag[F[X] forSome {type X}], atpe: WeakTypeTag[A]): c.Expr[MakeDef[F, A]] = {
    val makeTc = weakTypeOf[MakeDef[F, _]].typeConstructor
    val searchType = appliedType(makeTc, ftpe.tpe, atpe.tpe)

    c.inferImplicitValue(searchType) match {
      case EmptyTree =>
        c.abort(c.enclosingPosition, "Failed")
      case tree =>
        println(c.universe.showRaw(tree))
        tree.collect{
          case a: Apply => 
            println(s"This: $a")
        }
        println(tree)
        c.info(c.enclosingPosition, "OK! Replace debug", true)
        c.Expr[MakeDef[F, A]](tree)
    }
  }

  // def materializeDef: c.Expr[Make.Def] = {
  //   val enclosing = c.enclosingImplicits
  //   if (enclosing.size < 2) {
  //     c.abort(c.enclosingPosition, "Make.Def might be used only in `implicit def`")
  //   }
  //   if (state.debug) {
  //     println(enclosing)
  //   }
  //   c.Expr[Make.Def](q"_root_.make.Make.Def.instance")
  // }
  
  def debug[F[_], A](
    implicit ftpe: WeakTypeTag[F[X] forSome {type X}], atpe: WeakTypeTag[A]
  ): c.Expr[MakeDef[F, A]] = {

    val makeTc = weakTypeOf[MakeDef[F, _]].typeConstructor
    val searchType = appliedType(makeTc, ftpe.tpe, atpe.tpe)

    state.debug = true
    val out = c.inferImplicitValue(searchType)
    state.debug = false
    out match {
      case EmptyTree => 
        c.abort(c.enclosingPosition, "Failed")
      case tree => 
        val message = s"Debug: OK!\n\tMake instance for ${atpe.tpe} exists.\n\nRemove debug usage."
        c.info(c.enclosingPosition, message, true)
        c.Expr[MakeDef[F, A]](tree)
    }
  }

  def debugHook[F[_], A](
    implicit ftpe: WeakTypeTag[F[X] forSome {type X}], atpe: WeakTypeTag[A]
  ): c.Expr[Debug[MakeDef[F, A]]] = {

    if (!state.debug) c.abort(c.enclosingPosition, "debug is not enabled")

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

      state.traces.get(atpe.tpe) match {
        case Some(traces) =>
          val contains = traces.contains(Trace(open))
          println(s"CONTAINES? $contains ${open.mkString("\n")}")
          state.traces.update(atpe.tpe, traces + Trace(open))
        case None =>
          state.traces.update(atpe.tpe, Set(Trace(open)))
      }

      val makeDef = weakTypeOf[MakeDef[F, _]].typeConstructor
      val makeTpe = appliedType(makeDef, ftpe.tpe, atpe.tpe)

      val x = c.inferImplicitValue(makeTpe)
      x match {
        case EmptyTree =>

          c.info(c.enclosingPosition, s"REACHED EXPORT HOOK ${atpe.tpe}", true)
          c.abort(c.enclosingPosition, "silent error")
        case smt =>
          c.abort(c.enclosingPosition, "Instance exists: skip")
      }

    }
  }

  case class Trace(path: List[c.ImplicitCandidate])
  class DebugSt(
    var debug: Boolean = false,
    val traces: mutable.HashMap[Type, Set[Trace]] = mutable.HashMap.empty
  )
}