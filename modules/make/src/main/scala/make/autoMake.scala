package make

import scala.reflect.macros.blackbox
import scala.annotation.StaticAnnotation
import make.internal.MakeAnnotationMacro
import scala.annotation.compileTimeOnly

class autoMake extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro MakeAnnotationMacro.autoMake
}