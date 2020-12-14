package make.internal

import scala.reflect.macros.blackbox
import make.Tag.TpeTag
import make.Tag

class TpeTagMacro(val c:blackbox.Context) {

  import c.universe._

  def materializeTpeTag[A : WeakTypeTag]: c.Expr[TpeTag[A]] = {
    val tpe = weakTypeOf[A]
    val tree = q"_root_.make.Tag.TpeTag[${tpe}](${transformTpe(tpe)})"
    c.Expr[TpeTag[A]](tree)
  }

  private def transformTpe(t: c.Type): c.Tree = {
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
              case n =>
                None
            }
          case x if x.typeSymbol.isParameter =>
            val tagTpe = appliedType(weakTypeOf[Tag.TPTag[X] forSome {type X}].typeConstructor, t)
            optionFromImplicitTree(c.inferImplicitValue(tagTpe))
              .map{t => q"$t.symbol"}
          case x => None
        }
      } else {
         Some(q"${normalized.typeSymbol.fullName}")
      }
    
    resolved match {
      case None => c.abort(c.enclosingPosition, s"Not found Tag for $t")
      case Some(tree) =>
        val arguments = t.typeArgs.map(transformTpe)
        q"_root_.make.Tag.TpeTag.Type($tree, List(..$arguments))"
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

    val tpe = weakTypeTag.tpe.finalResultType
    tpe match {
      case ref: TypeRef =>
         val symbol = ref.sym 
         val tree = q"""_root_.make.Tag.TCTag[${weakTypeTag.tpe}](${symbol.fullName})"""
         c.Expr[Tag.TCTag[F]](tree)
      case x => 
        c.warning(c.enclosingPosition, s"Failed to create Tag.TCTag for $tpe")
        c.abort(c.enclosingPosition, "Failed to make TCTag")
    }
  }

  def materializeTPTag[A](implicit
    weakTypeTag: WeakTypeTag[A]
  ): c.Expr[Tag.TPTag[A]] = {
    val tpe = weakTypeTag.tpe.dealias.etaExpand
    val symbol = tpe.typeSymbol
    if (symbol.isParameter) {
      c.abort(c.enclosingPosition, "Failed to make TCTag")
    } else {
      val tree = q"""_root_.make.Tag.TPTag[${weakTypeTag.tpe}](${symbol.fullName})"""
      c.Expr[Tag.TPTag[A]](tree)
    }
  }

}