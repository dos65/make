package make.internal

import scala.reflect.macros.blackbox
import make.Tag.SourcePos

class SourcePosMacro(val c: blackbox.Context) {

  import c.universe._

  def materializeSourcePos: c.Expr[SourcePos] = {
    val owner = c.internal.enclosingOwner
    val pos = c.enclosingPosition
    val line = pos.line
    val start = pos.column
    val tree = q"new _root_.make.Tag.SourcePos(${owner.fullName}, $line, $start)"
    c.Expr[SourcePos](tree)
  }

}
