package make.internal

import scala.reflect.macros.whitebox
import make.Make
import make.MakeDef
import make.GenFailed

class DebugSt(
  var debug: Boolean = false
)

class MakeMacro(val c: whitebox.Context) {

  import c.universe._

  val state = MacroState.getOrElseUpdate[DebugSt](c.universe, new DebugSt)

  def debugDerive[F[_], A](implicit ftpe: WeakTypeTag[F[X] forSome {type X}], atpe: WeakTypeTag[A]): c.Expr[Make[F, A]] = {
    val makeTc = weakTypeOf[Make[F, _]].typeConstructor
    state.debug = true
    val searchType = appliedType(makeTc, ftpe.tpe, atpe.tpe)

    c.inferImplicitValue(searchType) match {
      case EmptyTree =>
        state.debug = false
        c.abort(c.enclosingPosition, "Failed")
      case tree =>
        state.debug = false
        c.info(c.enclosingPosition, "OK! Replace debug", true)
        c.Expr[Make[F, A]](tree)
    }
  }

  def materializeDef: c.Expr[Make.Def] = {
    val enclosing = c.enclosingImplicits
    if (enclosing.size < 2) {
      c.abort(c.enclosingPosition, "Make.Def might be used only in `implicit def`")
    }
    if (state.debug) {
      println(enclosing)
    }
    c.Expr[Make.Def](q"_root_.make.Make.Def.instance")
  }
  
  def genFailed[F[_], A](implicit ftpe: WeakTypeTag[F[X] forSome {type X}], atpe: WeakTypeTag[A]): c.Expr[MakeDef[F, A]] = {
    println(c.openImplicits)
    val gen = weakTypeOf[GenFailed[_]].typeConstructor
    val makeDef = weakTypeOf[MakeDef[F, _]].typeConstructor
    val makeTpe = appliedType(makeDef, ftpe.tpe, atpe.tpe)

    //val tpe = appliedType(gen, ftpe.tpe, atpe.tpe)
    
    val x = c.inferImplicitValue(makeTpe, withMacrosDisabled = true)
    println(x)
    x match {
      case EmptyTree => 
        val tree = q"_root_.make.MakeDef.FullyFailed[${ftpe.tpe}, ${atpe.tpe}]"
        c.Expr[MakeDef[F, A]](tree)
      case tree =>
        c.abort(c.enclosingPosition, "skip")
        //c.Expr[MakeDef[F, A]](tree)
    }

  }
}