package make.internal

import scala.reflect.macros.blackbox

class MakeAnnotationMacro(val c: blackbox.Context) {

  import c.universe._

  def deriveMake(annottees: Tree*): Tree = {
    annottees match {
    case List(
           clsDef @ q"case class ${name: TypeName}[..${typeParams: Seq[TypeDef]}](..${valueParams: Seq[ValDef]})(implicit ..${implicitParams}) extends ..$bases { ..$body }" 
         ) =>
          q"""
            $clsDef
            object ${name.toTermName} {
              ${instanceTree(name, typeParams, valueParams, implicitParams)}
            } 
           """
    case List(
           clsDef @ q"case class ${name: TypeName}[..${typeParams: Seq[TypeDef]}](..${valueParams: Seq[ValDef]})(implicit ..${implicitParams}) extends ..$bases { ..$body }",
           q"..$mods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
         ) =>
          q"""
            $clsDef
            $mods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf =>
               ..$objDefs
              ..${instanceTree(name, typeParams, valueParams, implicitParams)}
            }
           """
    case _ =>
      c.abort(c.enclosingPosition, "@deriveMake can be applied only on case classes")
    }
  }

  private def instanceTree(
    name: TypeName,
    typeParams: Seq[TypeDef],
    params: Seq[ValDef],
    implicitParams: Seq[ValDef]
  ): Tree = {
    val paramsTpe = params.map(_.tpt)
    val tpe = tq"(..${params.map(_.tpt)})"
    val typeArgs = typeParams.map(_.name)
    val applyF = params.size match {
      case 1 => q"${name.toTermName}.apply[..$typeArgs]"
      case _ => q"(${name.toTermName}.apply[..$typeArgs] _).tupled"
    }
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
      
    q"""
        implicit def make[$effTpe[_], ..$typeParams](
            implicit ..${implicits}
        ): Make[$effTpe, $targetTpe] = deps.map($applyF)
      """
  }

}