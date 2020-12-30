package make.internal

import scala.reflect.macros.blackbox
import make.Tag.TpeTag
import make.Tag

class TpeTagMacro(val c: blackbox.Context) {

  import c.universe._

  def materializeTpeTag[A : WeakTypeTag]: c.Expr[TpeTag[A]] = {
    val tpe = weakTypeOf[A]
    val value = transformTpe3(tpe)
    val tree = q"_root_.make.Tag.TpeTag[${tpe}]($value)"
    c.Expr[TpeTag[A]](tree)
  }

  private def transformTpe3(t: c.Type): c.Tree = {
    if (t.typeSymbol.isParameter)
      transformTpeParameter(t)
    else
      tranformDefinedTpe(t)
  }

  private def tranformDefinedTpe(t: c.Type): c.Tree = {
    val name = t.typeSymbol.fullName
    val nameTree = q"$name"
    processTypeParameters(nameTree, t)
  }

  private def processTypeParameters(symbolName: c.Tree, t: c.Type): c.Tree = {
    val inner = t.dealias.typeArgs.map(transformTpe3)
    q"_root_.make.Tag.TpeTag.Type($symbolName, $inner)"
  }

  private def transformTpeParameter(t: c.Type): c.Tree = {
    t.etaExpand match {
      case tpe: PolyType =>
        tpe.typeParams.size match {
          case 1 => processTypeParameters(searchTCTag(t), t)
          case n => c.abort(c.enclosingPosition, s"Not implemented for type paramters where n=$n")
        }
      case _ => searchTPTag(t)
    }
  }


  private def searchTCTag(t: c.Type): c.Tree = {
    val tcTagTpe = appliedType(weakTypeOf[Tag.TCTag[X] forSome {type X[_]}].typeConstructor, t.typeConstructor)
    optionFromImplicitTree(c.inferImplicitValue(tcTagTpe))
      .map(t => q"$t.symbol")
      .getOrElse(c.abort(c.enclosingPosition, "Not implemented"))
  }

  private def searchTPTag(t: c.Type): c.Tree = {
    val tagTpe = appliedType(weakTypeOf[Tag.TPTag[X] forSome {type X}].typeConstructor, t)
    optionFromImplicitTree(c.inferImplicitValue(tagTpe))
      .map{t => q"$t.tpe"}
      .getOrElse(c.abort(c.enclosingPosition, "Not implemented"))
  }

  private def optionFromImplicitTree(tree: c.Tree): Option[c.Tree] =
    tree match {
      case EmptyTree => None
      case tree => Some(tree) 
    }

  def materializeTCTag[F[_]](implicit
    weakTypeTag: WeakTypeTag[F[X] forSome {type X}]
  ): c.Expr[Tag.TCTag[F]] = {

    val tpe = weakTypeTag.tpe
    tpe match {
      case tpe: PolyType => 
         val symbol = c.internal.fullyInitialize(tpe.resultType.typeSymbol)
         val name = fullName(symbol)
         val tree = q"""_root_.make.Tag.TCTag[${weakTypeTag.tpe}](${name})"""
         c.Expr[Tag.TCTag[F]](tree)
      case ref: TypeRef =>
         val symbol = ref.sym 
         val name = fullName(symbol)
         val tree = q"""_root_.make.Tag.TCTag[${weakTypeTag.tpe}](${name})"""
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
      c.abort(c.enclosingPosition, s"Failed to make TPTag for $tpe")
    } else {
      val tree = q"""_root_.make.Tag.TPTag[${weakTypeTag.tpe}](_root_.make.Tag.TpeTag.Type(${symbol.fullName}, List.empty))"""
      c.Expr[Tag.TPTag[A]](tree)
    }
  }

  private def fullName(s: c.Symbol): String = {
    def loop(sym: c.Symbol, acc: List[String]): String = {
      val name = sym.name.decodedName.toString
      if (sym == NoSymbol || name == "<root>") acc.mkString(".")
      else {
        // see test case with cats.Id
        val skip =
          name.startsWith("<local ") || name == "package" || (sym.isAbstract && sym.isParameter)
        val next = if (skip) acc else name :: acc
        loop(sym.owner, next)
      }
    }
    loop(s, List.empty)
  }

}