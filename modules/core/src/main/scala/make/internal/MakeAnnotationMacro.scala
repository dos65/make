package make.internal

import scala.reflect.macros.blackbox
import scala.reflect.internal.Flags
import scala.annotation.tailrec

class MakeAnnotationMacro(val c: blackbox.Context) {

  import c.universe._

  def autoMake(annottees: Tree*): Tree = {
    annottees match {
      case List(cls: ClassDef) =>
        Clz.extract(cls) match {
          case Some(clz) =>
            q"""
              $cls
              object ${cls.name.toTermName} {
                ${instanceTree(clz)}
              }
            """
          case None => reportUnexpectedError()
        }
      case List(
            cls: ClassDef,
            q"..$mods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
          ) =>
        Clz.extract(cls) match {
          case Some(clz) =>
            q"""
                $cls
                $mods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf =>
                  ..$objDefs
                  ..${instanceTree(clz)}
                }
              """
          case None => reportUnexpectedError()
        }
      case _ =>
        c.abort(c.enclosingPosition, "@deriveMake can be applied only on case classes or classes")
    }
  }

  private def reportUnexpectedError(): Nothing =
    c.abort(c.enclosingPosition, "Something went wrong. Please file an issue")

  private def instanceTree(clz: Clz): Tree = {
    import clz._

    val effTpe = TermName(c.freshName("E")).toTypeName
    val paramsTpe = params.map(_.tpt)

    val dependencies: List[(TermName, c.Tree)] = params.zipWithIndex.map{case (dp, i) =>
      val name = TermName(c.freshName(s"dep$i"))
      val tree = q"$name: _root_.make.Make[$effTpe, ${dp.tpt}]"
      (name, tree)
    }
    val implicitDependencies = dependencies.map(_._2)

    val targetTpe =
      if (typeParams.isEmpty)
        tq"${name.toTypeName}"
      else
        tq"${name.toTypeName}[..${typeParams.map(_.name)}]"
    
    val impl =
      if (dependencies.isEmpty) {
        q"_root_.make.Make.pure[$effTpe, ${targetTpe}]($create)"
      } else {
        dependencies.reverse.map(_._1).foldLeft(EmptyTree){
          case (EmptyTree, name) => q"_root_.make.internal.MakeOps.map($name)($create)" 
          case (tree, name) =>  q"_root_.make.internal.MakeOps.ap($name)($tree)"
        }
      }

    val implicits =
        q"${TermName(c.freshName())}: _root_.cats.Applicative[$effTpe]" ::
        implicitDependencies ++
        implicitParams.toList ++
        typeParams.zipWithIndex.flatMap{ case (t, i) =>
          tagFor(t).map{tagTpe =>
            val name = TermName(c.freshName(s"tpeTag$i"))
            q"$name: $tagTpe"
          }
        } ++
        (if (paramsTpe.isEmpty) List.empty else List(q"tag: _root_.make.Tag[$targetTpe]"))

    q"""
        implicit def make[$effTpe[_], ..$typeParams](
            implicit ..${implicits}
        ): _root_.make.Make[$effTpe, $targetTpe] =
          $impl
      """
  }

  case class DepParam(valDef: ValDef, tree: Tree, tpt: Tree)
  object DepParam {

    def create(v: ValDef, i: Int): DepParam = {
      val name = TermName(s"x$i")
      val (tree, tpt) =
        v.mods.annotations match {
          case anno :: _ =>
            val annoTpe = annotationTpe(anno)
            val tpt = tq"_root_.make.annotated.:@:[${v.tpt}, $annoTpe]"
            val tree = Select(Ident(name), TermName("value"))
            (tree, tpt)
          case Nil => 
            (Ident(name), v.tpt)
        }
       val valDef = ValDef(Modifiers(Flag.PARAM), TermName(s"x$i"), tpt, EmptyTree)
       DepParam(valDef, tree, tpt)
    }


    private def annotationTpe(tree: c.Tree): c.Tree = {
      tree match {
        case Apply(Select(New(annoSelect), _), _) =>
          annoSelect
        case _ =>
          c.abort(c.enclosingPosition, "Annotation descontruction failed")
      }
    }
  }

  // TODO
  private def tagFor(typeDef: TypeDef): Option[Tree] = {
    typeDef.tparams.size match {
      case 0 => None
      case 1 => Some(tq"_root_.make.Tag.TCTag[${typeDef.name}]")
      case _ => None
    }
  }

  case class Clz(
    name: TypeName,
    typeParams: List[TypeDef],
    params: List[DepParam],
    implicitParams: List[ValDef],
    create: Tree
  )

  object Clz {

    def extract(clsDef: ClassDef): Option[Clz] = {
      findInit(clsDef.impl.body).map { init =>
        val tparams = clsDef.tparams
        val (params, implicitParams) =
          init.vparamss.flatten.foldLeft((Vector.empty[ValDef], Vector.empty[ValDef])) {
            case ((pAcc, ipAcc), vdef) =>
              val isImplicit = vdef.mods.hasFlag(Flag.IMPLICIT)
              if (isImplicit)
                (pAcc, ipAcc :+ vdef)
              else
                (pAcc :+ vdef, ipAcc)
          }

        val depParams =   
          params.zipWithIndex
            .map({ case (d, i) =>
              DepParam.create(d, i)
            })
            .toList
        val create = createFunction(depParams, clsDef, init)

        Clz(clsDef.name, tparams, depParams, implicitParams.toList, create)
      }
    }

    def createFunction(
      params: List[DepParam],
      clsDef: ClassDef,
      init: DefDef
    ): Tree = {

      @tailrec
      def toLambda(in: List[DepParam], acc: Tree): Tree = {
        in match {
          case head :: tl => 
            val nextAcc = Function(List(head.valDef), acc)
            toLambda(tl, nextAcc)
          case Nil => acc
        }
      }

      val initAcc = 
        Apply(
          Select(New(Ident(clsDef.name.decodedName)), init.name),
          params.map(d => d.tree)
        )

      toLambda(params, initAcc)
    }

    def findInit(body: List[Tree]): Option[DefDef] =
      body.collectFirst {
        case defdef: DefDef if defdef.name.decodedName.toString == "<init>" => defdef
      }

  }
}
