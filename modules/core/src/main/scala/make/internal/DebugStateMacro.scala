package make.internal

import scala.reflect.macros.whitebox
import scala.collection.mutable

abstract class DebugStateMacro(val c: whitebox.Context) {
  import c.universe._

  val debugState = MacroState.getOrElseUpdate[DebugSt](c.universe, new DebugSt)

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
