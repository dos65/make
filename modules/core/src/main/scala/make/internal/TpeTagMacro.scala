package make.internal

import scala.reflect.macros.blackbox
import make.Tag.TpeTag
import make.Tag
import scala.reflect.internal.Constants

class TpeTagMacro(val c: blackbox.Context) {

  import c.universe._

  def materializeTpeTag[A : WeakTypeTag]: c.Expr[TpeTag[A]] = {
    val tpe = weakTypeOf[A]
    println(s"CALL MATERIALIZE: $tpe ${tpe.typeSymbol.isParameter}")
    val value = transformTpe3(tpe)
    val tree = q"_root_.make.Tag.TpeTag[${tpe}]($value)"
    println(s"OK!! $tree")
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
    // t.dealias match {
    //   case ref: TypeRef =>
    //     val inner = ref.args.map(transformTpe3)
    //     q"_root_.make.Tag.TpeTag.Type($symbolName, $inner)"
    //   case x => 
    //     c.info(c.enclosingPosition, s"TpeTag not implemented for $x: ${x.getClass}", true)
    //     c.abort(c.enclosingPosition, "Not implemented")
    // }
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
    // val kind = t.dealias.typeArgs
    // println(s"TYPE ARGS for $t : $kind")
    // kind.size match {
    //   case 0 => searchTPTag(t)
    //   case 1 => processTypeParameters(searchTCTag(t), t)
    //   case n => c.abort(c.enclosingPosition, "Not implemented")
    // }

    // t.dealias match {
    //   case tpe: TypeRef =>
    //     val inner = tpe.args.map(transformTpe3)
    //     q"_root_.make.Tag.TpeTag.Type(${symbolTree}, $inner)"
    //   case x => 
    //     c.info(c.enclosingPosition, s"TpeTag(parameter) not implemented for $x: ${x.getClass}", true)
    //     c.abort(c.enclosingPosition, "Not implemented")
    // }
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

  // private def transformTpe2(t: c.Type): c.Tree = {
  //   println(s"transform $t ${t.typeSymbol.isParameter} ${t.getClass}")
  //   val normalized = t
  //   if (t.typeSymbol.isParameter) {
  //        t.dealias match {
  //         case ref: PolyType => 
  //           println(s"IS POLY ${ref.typeParams} ${ref.typeArgs}")
  //           val kind = math.max(ref.typeParams.size, ref.typeArgs.size)
  //           ref.typeParams.size match {
  //             case 0 =>
  //               val inner = ref.typeArgs.map(transformTpe2)
  //               q"_root_.make.Tag.TpeTag.Type(${ref.typeSymbol.fullName}, $inner)"
  //             case 1 => 
  //               val tcTagTpe = appliedType(weakTypeOf[Tag.TCTag[X] forSome {type X[_]}].typeConstructor, t.typeConstructor)
  //               optionFromImplicitTree(c.inferImplicitValue(tcTagTpe))
  //                 .map(t => q"$t.tpe")
  //                 .getOrElse(c.abort(c.enclosingPosition, "Not implemented"))
  //             case n =>
  //                c.abort(c.enclosingPosition, "Not implemented")
  //           }
  //         case ref: TypeRef =>
  //           ref.typeParams.size match {
  //             case 0 =>
  //               val inner = ref.typeArgs.map(transformTpe2)
  //               q"_root_.make.Tag.TpeTag.Type(${ref.typeSymbol.fullName}, $inner)"
  //             case 1 => 
  //               val tcTagTpe = appliedType(weakTypeOf[Tag.TCTag[X] forSome {type X[_]}].typeConstructor, t.typeConstructor)
  //               optionFromImplicitTree(c.inferImplicitValue(tcTagTpe))
  //                 .map(t => q"$t.tpe")
  //                 .getOrElse(c.abort(c.enclosingPosition, "Not implemented"))
  //             case n =>
  //                c.abort(c.enclosingPosition, "Not implemented")
  //           }
            
  //         // case PolyType(typeParams, _) =>
  //         //   println(s"POLY TYPE: $typeParams")
  //         //   typeParams.size match {
  //         //     case 1 => 
  //         //       val tcTagTpe = appliedType(weakTypeOf[Tag.TCTag[X] forSome {type X[_]}].typeConstructor, t.typeConstructor)
  //         //       optionFromImplicitTree(c.inferImplicitValue(tcTagTpe))
  //         //         .map(t => q"$t.tpe")
  //         //         .getOrElse(c.abort(c.enclosingPosition, "Not implemented"))
  //         //     case n =>
  //         //      c.abort(c.enclosingPosition, "Not implemented")
  //         //   }
  //         // case x if x.typeSymbol.isParameter =>
  //         //   val tagTpe = appliedType(weakTypeOf[Tag.TPTag[X] forSome {type X}].typeConstructor, t)
  //         //   optionFromImplicitTree(c.inferImplicitValue(tagTpe))
  //         //     .map{t => q"$t.tpe"}
  //         //     .getOrElse(c.abort(c.enclosingPosition, "Not implemented"))
  //         case x =>
  //           c.abort(c.enclosingPosition, "Not implemented")
  //       }
  //   } else {
  //     t match {
  //       case ref: TypeRef =>
  //         val inner = ref.args.map(transformTpe2)
  //         q"_root_.make.Tag.TpeTag.Type(${ref.typeSymbol.fullName}, $inner)"
  //       case x => 
  //         c.info(c.enclosingPosition, s"TpeTag not implemented for $x: ${x.getClass}", true)
  //         c.abort(c.enclosingPosition, "Not implemented")
  //     }
  //   }
  // }

  // private def transformTpe(t: c.Type): c.Tree = {
  //   val normalized = t.dealias.etaExpand
  //   println(normalized + " " + normalized.getClass)
  //   val resolved = 
  //     if (normalized.typeSymbol.isParameter) {
  //       normalized match {
  //         case PolyType(typeParams, _) =>
  //           println(s"POLY TYPE: $typeParams")
  //           typeParams.size match {
  //             case 1 => 
  //               val tcTagTpe = appliedType(weakTypeOf[Tag.TCTag[X] forSome {type X[_]}].typeConstructor, t.typeConstructor)
  //               optionFromImplicitTree(c.inferImplicitValue(tcTagTpe))
  //                 .map(t => q"$t.tpe")
  //             case n =>
  //               None
  //           }
  //         case x if x.typeSymbol.isParameter =>
  //           val tagTpe = appliedType(weakTypeOf[Tag.TPTag[X] forSome {type X}].typeConstructor, t)
  //           optionFromImplicitTree(c.inferImplicitValue(tagTpe))
  //             .map{t => q"$t.tpe"}
  //         case x => None
  //       }
  //     } else {
  //        Some(q"${normalized.typeSymbol.fullName}")
  //     }
    
  //   resolved match {
  //     case None => c.abort(c.enclosingPosition, s"Not found Tag for $t")
  //     case Some(tree) =>
  //       tree
  //       // val arguments = t.typeArgs.map(transformTpe)
  //       // q"_root_.make.Tag.TpeTag.Type($tree, List(..$arguments))"
  //   }
  // }

  private def optionFromImplicitTree(tree: c.Tree): Option[c.Tree] =
    tree match {
      case EmptyTree => None
      case tree => Some(tree) 
    }

  def materializeTCTag[F[_]](implicit
    weakTypeTag: WeakTypeTag[F[X] forSome {type X}]
  ): c.Expr[Tag.TCTag[F]] = {

    val tpe = weakTypeTag.tpe
    println(s"TCTAG: ${tpe} ${tpe.getClass}")
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
        // val skip = false
        val next = if (skip) acc else name :: acc
        loop(sym.owner, next)
      }
    }
    println(s.fullName)
    loop(s, List.empty)
  }

}