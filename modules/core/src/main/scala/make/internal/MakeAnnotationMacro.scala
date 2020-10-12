package make.internal

import scala.reflect.macros.blackbox
import scala.reflect.internal.Flags

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

    val paramsTpe = params.map(_.tpt)
    val tpe = tq"(..${params.map(_.tpt)})"
    val typeArgs = typeParams.map(_.name)

    val mapF = if (params.size > 1) q"($create).tupled" else create

    val effTpe = TermName(c.freshName("E")).toTypeName

    val targetTpe =
      if (typeParams.isEmpty)
        tq"${name.toTypeName}"
      else
        tq"${name.toTypeName}[..${typeParams.map(_.name)}]"

    val implicits =
      q"deps: _root_.make.Make[$effTpe, $tpe]" ::
        q"${TermName(c.freshName())}: _root_.cats.Applicative[$effTpe]" ::
        implicitParams.toList ++
        (if (paramsTpe.isEmpty) List.empty else List(q"tag: _root_.make.Tag[$targetTpe]"))

    q"""
        implicit def make[$effTpe[_], ..$typeParams](
            implicit ..${implicits}
        ): Make[$effTpe, $targetTpe] =
          _root_.make.internal.MakeOps.map(deps)($mapF)
      """
  }

  case class Clz(
    name: TypeName,
    typeParams: List[TypeDef],
    params: List[ValDef],
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

        val create = {
          val funValDefs = params.zipWithIndex
            .map({ case (d, i) =>
              ValDef(Modifiers(Flag.PARAM), TermName(s"x$i"), d.tpt, EmptyTree)
            })
            .toList
          Function(
            funValDefs,
            Apply(
              Select(New(Ident(clsDef.name.decodedName)), init.name),
              funValDefs.map(d => Ident(d.name))
            )
          )
        }

        Clz(clsDef.name, tparams, params.toList, implicitParams.toList, create)
      }
    }

    def findInit(body: List[Tree]): Option[DefDef] =
      body.collectFirst {
        case defdef: DefDef if defdef.name.decodedName.toString == "<init>" => defdef
      }

  }
}
