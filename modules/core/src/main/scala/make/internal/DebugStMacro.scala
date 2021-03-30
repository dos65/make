package make.internal

import scala.reflect.macros.whitebox
import scala.collection.mutable

abstract class DebugStMacro(val c: whitebox.Context) {

  val state = MacroState.getOrElseUpdate[DebugSt](c.universe, new DebugSt)

  sealed trait ResolveSt
  object ResolveSt {
    case object InProgress extends ResolveSt
    case class Resolved(tree: c.Tree) extends ResolveSt
  }

  class DebugSt(
    var debug: Boolean = false,
    val stack: mutable.ListBuffer[c.Type] = mutable.ListBuffer.empty,
    val resolveCache: mutable.HashMap[c.Type, ResolveSt] = mutable.HashMap.empty,
    val reverseTraces: mutable.HashMap[c.Type, List[c.Type]] =
      mutable.HashMap.empty
  ) {

    def registerFailedReason(targetTpe: c.Type, prev: c.Type): Unit = {
      val curr = reverseTraces.getOrElse(prev, List.empty)
      val next = targetTpe :: curr
      reverseTraces.update(prev, next)
    }

  }
}
