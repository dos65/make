package make.internal

//import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.whitebox.Context
import make.Make

// class FailedTraces(
//   var: 
// )

class DepsCtxMacros(val c: Context) {

  import c.universe._

  // private def mkError(failedTpe: Type, path: List[c.ImplicitCandidate]): String = {
  //   val sb = new StringBuilder()
  //   // path.reverse.foldLeft(sb){case (acc, candidate) => 
  //   //   val entry = s"${candidate.}"
  //   // }
  //   ???
  // }

  def materializeDeps[F[_], A](implicit ftpe: WeakTypeTag[F[X] forSome {type X}], atpe: WeakTypeTag[A]): c.Expr[Make.DepsCtx[F, A]] = {
    val makeTc = weakTypeOf[Make[F, _]].typeConstructor
    val out = resolveMake(makeTc, ftpe.tpe, atpe.tpe).orElse(tryTuple(makeTc, ftpe.tpe, atpe.tpe))
    
    // val xxx = c.openImplicits
    //   .filter(x => {
    //     val name = x.pt.typeSymbol.fullName
    //     name == "make.Make.DepsCtx" || name == "make.Make"
    //   }).map(candidate => {
    //     candidate.pt.typeArgs
    //   })


    // println("Deps:" + xxx.mkString("\n"))
    out match {
      case Some(tree) =>
        val ctxTree =
          q"""
            new _root_.make.Make.DepsCtx[${ftpe.tpe}, ${atpe.tpe}]($tree)
          """
        c.Expr[Make.DepsCtx[F, A]](ctxTree)
      case None =>
        c.abort(c.enclosingPosition, s"Can't find `Make` for ${atpe.tpe}")
    }
  }

  def materializeMake[F[_], A](implicit ftpe: WeakTypeTag[F[X] forSome {type X}], atpe: WeakTypeTag[A]): c.Expr[Make[F, A]] = {
    val makeTc = weakTypeOf[Make[F, _]].typeConstructor
    val out = resolveMake(makeTc, ftpe.tpe, atpe.tpe).orElse(tryTuple(makeTc, ftpe.tpe, atpe.tpe))
    
    out match {
      case Some(tree) => c.Expr[Make[F, A]](tree)
      case None =>
       c.abort(c.enclosingPosition, s"Can't find `Make` for ${atpe.tpe}")
    }
  }

  private val tupleRegex = "scala.Tuple(\\d+)".r

  private def tryTuple(makeTc: Type, ftpe: Type, tpe: Type): Option[c.Tree] = {
    val name = tpe.typeSymbol.fullName
    
    name match {
      case tupleRegex(x) =>
        val init = List.empty[String]
        val (nodes, errors) = tpe.typeArgs.foldLeft((List.empty[Tree], List.empty[String])){
          case ((acc, errAcc), t) =>
            resolveMake(makeTc, ftpe, t) match {
              case Some(tree) => (q"$tree.node" :: acc, errAcc)
              case None =>
                val err = s"Can't find `Make` for ${t}" :: errAcc
                (acc, err)
            }
        }
        if (errors.nonEmpty) {
          val x = errors.mkString("\n")
          c.error(c.enclosingPosition, x)
          None
        } else {
          val nodeTree = q"_root_.make.internal.NodeOps.product(..${nodes.reverse})"
          val make =
           q"""
            new _root_.make.Make[$ftpe, $tpe] {
              def node: _root_.make.internal.Node[${ftpe}, $tpe] = $nodeTree
            }
            """
          Some(make)
        }
      case _ => None
    }
  }

  private def resolveMake(makeTc: Type, ftpe: Type, atpe: Type): Option[c.Tree] = {
    val searchType = appliedType(makeTc, ftpe, atpe)
    c.inferImplicitValue(searchType) match {
      case EmptyTree => None
      case tree => Some(tree) 
    }
  }
}