package make

import scala.reflect.macros.blackbox
import scala.annotation.StaticAnnotation
import make.internal.MakeAnnotationMacro
import scala.annotation.compileTimeOnly

class deriveMake extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro MakeAnnotationMacro.deriveMake
}