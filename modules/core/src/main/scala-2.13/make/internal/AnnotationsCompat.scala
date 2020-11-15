package make.internal

import scala.reflect.macros.blackbox

trait AnnotationsCompat {

  val c: blackbox.Context

  import c.universe._

  def annotationTpe(tree: c.Tree): c.Tree = {
    tree match {
      case Apply(Select(New(annoSelect), _), _) =>
        annoSelect
      case _ =>
        c.abort(c.enclosingPosition, "Annotation descontruction failed")
    }
  }
}