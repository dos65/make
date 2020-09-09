package make.internal

import scala.reflect.macros.blackbox

class MakeAnnotationMacro(val c: blackbox.Context) {

  import c.universe._

  def deriveMake(annottees: Tree*): Tree = {
    annottees match {
    case List(
           clsDef @ q"case class ${name: TypeName}[..${typeParams: Seq[TypeDef]}](..${valueParams: Seq[ValDef]}) extends ..$bases { ..$body }" 
         ) =>
          q"""
            $clsDef
            object ${name.toTermName} {
              ${instanceTree(name, valueParams)}
            } 
           """
    case List(
           clsDef @ q"case class ${name: TypeName}[..${typeParams: Seq[TypeDef]}](..${valueParams: Seq[ValDef]}) extends ..$bases { ..$body }",
           q"..$mods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf => ..$objDefs }"
         ) =>
          q"""
            $clsDef
            $mods object $objName extends { ..$objEarlyDefs } with ..$objParents { $objSelf =>
               ..$objDefs
              ..${instanceTree(name, valueParams)}
            }
           """
    case _ =>
      c.abort(c.enclosingPosition, "@deriveMake can be applied only on case classes")
    }
  }

  private def instanceTree(
    name: TypeName,
    params: Seq[ValDef]
  ): Tree = {
    val paramsTpe = params.map(_.tpt)
    val tpe = tq"(..${params.map(_.tpt)})"
    val applyF = params.size match {
      case 1 => q"${name.toTermName}.apply"
      case _ => q"(${name.toTermName}.apply _).tupled"
    }
    q"""
        implicit def make[F[_]: _root_.cats.Applicative](
            implicit deps: _root_.make.Make[F, $tpe]
        ): Make[F, ${name}] = deps.map($applyF)
      """
  }

}