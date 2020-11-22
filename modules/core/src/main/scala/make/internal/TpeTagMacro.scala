package make.internal

import scala.reflect.macros.blackbox
import make.Tag.TpeTag
import make.Tag.TpeTag2
import make.Tag

class TpeTagMacro(val c:blackbox.Context) {

  import c.universe._

  def materializeTpeTag[A : WeakTypeTag]: c.Expr[TpeTag[A]] = {
    val tpe = weakTypeOf[A]
    val tree: c.Tree = q"_root_.make.Tag.TpeTag[${tpe}](${transformTpe(tpe)})"
    println(tree)
    c.Expr[TpeTag[A]](tree)
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

  def materializeTpeTag2[A : WeakTypeTag]: c.Expr[TpeTag2[A]] = {
    val tpe = weakTypeOf[A]
    val tree = q"_root_.make.Tag.TpeTag2[${tpe}](${transformTpe2(tpe)})"
    c.Expr[TpeTag2[A]](tree)
  }

  private def transformTpe2(t: c.Type): c.Tree = {
    val normalized = t.dealias.etaExpand
    val resolved = 
      if (normalized.typeSymbol.isParameter) {
        normalized match {
          case PolyType(typeParams, _) =>
            typeParams.size match {
              case 1 => 
                val tcTagTpe = appliedType(weakTypeOf[Tag.TCTag[X] forSome {type X[_]}].typeConstructor, t.typeConstructor)
                optionFromImplicitTree(c.inferImplicitValue(tcTagTpe))
                  .map(t => q"$t.symbol")
              case n => None
            }
          case x if x.typeSymbol.isParameter =>
            val tagTpe = appliedType(weakTypeOf[Tag.TpeTag2[X] forSome {type X}].typeConstructor, t)
            optionFromImplicitTree(c.inferImplicitValue(tagTpe))
              .map{t => q"$t.render"}
          case x => None
        }
      } else {
         Some(q"${normalized.typeSymbol.fullName}")
      }
    resolved match {
      case None => c.abort(c.enclosingPosition, s"Not found Tag for $t")
      case Some(tree) =>
        val arguments = t.typeArgs.map(transformTpe2)
        q"_root_.make.Tag.TpeTag2.Type($tree, List(..$arguments))"
    }
  }

  private def optionFromImplicitTree(tree: c.Tree): Option[c.Tree] =
    tree match {
      case EmptyTree => None
      case tree => Some(tree) 
    }

  def materializeTCTag[F[_]](implicit
    weakTypeTag: WeakTypeTag[F[X] forSome {type X}]
  ): c.Expr[Tag.TCTag[F]] = {
    val tpe = weakTypeTag.tpe.dealias.etaExpand
    val symbol = tpe.typeSymbol
    if (symbol.isParameter) {
      c.abort(c.enclosingPosition, "Failed to make TCTag")
    } else {
      val tree = q"""_root_.make.Tag.TCTag[${weakTypeTag.tpe}](${symbol.fullName})"""
      c.Expr[Tag.TCTag[F]](tree)
    }
  }

}
