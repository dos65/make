package make.internal

import scala.reflect.macros.blackbox
import scala.reflect.internal.Flags

class MakeAnnotationMacro(val c: blackbox.Context) {

  import c.universe._

  def deriveMake(annottees: Tree*): Tree = {
    def reportUnexpectedError(): Nothing =
       c.abort(c.enclosingPosition, "Something went wrong. Please file an issue")

    annottees match {
    // case List(
    //        clsDef @ q"case class $name[..$typeParams](..$valueParams)(implicit ..$implicitParams) extends ..$bases { ..$body }" 
    //      ) =>
    //       q"""
    //         $clsDef
    //         object ${name.toTermName} {
    //           ${instanceTree(name, typeParams, valueParams, implicitParams)}
    //         } 
    //        """
    // // case List(
    // //        clsDef @ q"class $name[..$typeParams](..$valueParams)(implicit ..$implicitParams) extends ..$bases { ..$body }" 
    // //      ) =>
    // //       //  println(s"HEHRE  ${clsDef.asInstanceOf[ClassDef]}")
    // //        val x: ClassDef = clsDef.asInstanceOf[ClassDef]
    // //        println(x.impl.body.map(x => x.isDef + " " + x.getClass))
    // //       q"""
    // //         $clsDef
    // //         object ${name.toTermName} {
    // //           ${instanceTree(name, typeParams, valueParams, implicitParams)}
    // //         } 
    // //        """
    // case List(
    //        clsDef @ q"case class $name[..$typeParams](..$valueParams)(implicit ..$implicitParams) extends ..$bases { ..$body }",
    //        q"..$mods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
    //      ) =>
    //       q"""
    //         $clsDef
    //         $mods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf =>
    //            ..$objDefs
    //           ..${instanceTree(name, typeParams, valueParams, implicitParams)}
    //         }
    //        """
    case List(
           clsDef @ q"case class $name[..$typeParams](..$valueParams)(implicit ..$implicitParams) extends ..$bases { ..$body }" 
         ) =>
         val clz = Clz.forCaseClass(name, typeParams, valueParams, implicitParams)
         q"""
            $clsDef
            object ${name.toTermName} {
              ${instanceTree(clz)}
            } 
          """
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

  private def instanceTree(clz: Clz): Tree = {
    import clz._

    val paramsTpe = params.map(_.tpt)
    val tpe = tq"(..${params.map(_.tpt)})"
    val typeArgs = typeParams.map(_.name)

    val funValDefs = clz.params.zipWithIndex.map({
      case (d, i) => ValDef(Modifiers(Flag.PARAM), TermName(s"x$i"), d.tpt, EmptyTree) 
    })
    val constructorF = 
      Function(funValDefs, Apply(Select(New(Ident(clz.name.decodedName)), init.name), funValDefs.map(d => Ident(d.name))))

    val mapF = if (funValDefs.size > 1) q"($constructorF).tupled" else constructorF

    val applyF = init
    val effTpe = TermName(c.freshName("MakeEff")).toTypeName
    
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
      
    val x2= q"""
        implicit def make[$effTpe[_], ..$typeParams](
            implicit ..${implicits}
        ): Make[$effTpe, $targetTpe] =
          _root_.make.internal.MakeOps.map(deps)($mapF)
      """
    println(x2)
    x2
  }

  case class Clz(
    name: TypeName,
    typeParams: List[TypeDef],
    params: List[ValDef],
    implicitParams: List[ValDef],
    init: DefDef
  )

  object Clz {

    def extract(clsDef: ClassDef): Option[Clz] = {
        findInit(clsDef.impl.body).map{ init =>
          val tparams = clsDef.tparams
          val (params, implicitParams) =
            init.vparamss.flatten.foldLeft((Vector.empty[ValDef], Vector.empty[ValDef])){
              case ((pAcc, ipAcc), vdef) => 
                val isImplicit = vdef.mods.hasFlag(Flag.IMPLICIT)
                if (isImplicit) 
                  (pAcc, ipAcc :+ vdef)
                else
                  (pAcc :+ vdef, ipAcc)
            }

          Clz(clsDef.name, tparams, params.toList, implicitParams.toList, init)
        }
    }

    def forCaseClass(
      name: TypeName,
      typeParams: List[TypeDef],
      params: List[ValDef],
      implicitParams: List[ValDef],
    ): Clz = {
      val init = q"(${name.toTermName}.apply _)"
      Clz(name, typeParams, params, implicitParams, init)
    }

    def findInit(body: List[Tree]): Option[DefDef] =
      body.collectFirst {
        case defdef: DefDef if defdef.name.decodedName.toString == "<init>" => defdef
      }

  }
}