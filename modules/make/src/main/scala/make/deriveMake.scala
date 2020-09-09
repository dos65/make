package make

import scala.reflect.macros.blackbox
import scala.annotation.StaticAnnotation
import make.internal.MakeAnnotationMacro
import scala.annotation.compileTimeOnly

@compileTimeOnly("derieMake is compile time only")
class deriveMake(x: Boolean = false) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro MakeAnnotationMacro.deriveMake
}