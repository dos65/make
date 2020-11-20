package make.internal

// import scala.reflect.macros.blackbox
import scala.reflect.macros.whitebox
import make.Tag.TpeTag

class TpeTagMacro(val c: whitebox.Context) {

  import c.universe._

  def materializeTpeTag[A : WeakTypeTag]: c.Expr[TpeTag[A]] = {
    val tpe = weakTypeOf[A]
    c.Expr[TpeTag[A]](q"_root_.make.Tag.TpeTag[${tpe}](${transformTpe(tpe)})")
  }

  private def transformTpe(t: c.Type): c.Tree = {
    val symbol = t.dealias.typeSymbol
    val name = symbol.fullName
    val args = t.typeArgs.map(transformTpe)
    val argsTree = q"List(..$args)"
    if (t.typeSymbol.isParameter) {
      q"_root_.make.Tag.TpeTag.Type.Parameter($name, $argsTree)"
    } else {
      q"_root_.make.Tag.TpeTag.Type.Stable($name, $argsTree)"
    }
  }

}
