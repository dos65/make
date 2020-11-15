package make.internal

import scala.reflect.macros.blackbox

trait AnnotationsCompat {

  val c: blackbox.Context

  import c.universe._

  def annotationTpe(tree: c.Tree): c.Tree =
    c.typecheck(tree).tpe
}