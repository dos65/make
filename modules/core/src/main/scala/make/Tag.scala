package make

import scala.reflect.runtime.universe.TypeTag
import scala.reflect.runtime.universe.WeakTypeTag
import make.internal.SourcePosMacro
import make.internal.TpeTagMacro

final case class Tag[A](
  typeTag: Tag.TpeTag[A],
  sourcePos: Tag.SourcePos,
)

object Tag {

  def of[A](implicit tag: Tag[A]): Tag[A] = tag

  implicit def tagFor[A](implicit typeTag: TpeTag[A], sourcePos: SourcePos): Tag[A] =
    Tag(typeTag, sourcePos)

  final case class SourcePos(path: String, line: Int, start: Int)
  object SourcePos {
    implicit def materialize: SourcePos =
      macro SourcePosMacro.materializeSourcePos
  }

  final case class TpeTag[A](tpe: TpeTag.Type)
  
  final case class TPTag[A](tpe: TpeTag.Type)
  object TPTag {
    implicit def materialize[A]: TPTag[A] =
      macro TpeTagMacro.materializeTPTag[A]
  }
  final case class TCTag[F[_]](symbol: String)
  object TCTag {
    implicit def materialize[F[_]]: TCTag[F] =
      macro TpeTagMacro.materializeTCTag[F]
  }

  object TpeTag {

    def apply[A](implicit tag: TpeTag[A]): TpeTag[A] = tag

    final case class Type(
      symbol: String,
      arguments: List[Type] 
    ) {

      override def toString: String = render

      def render: String = {
        val argsStr = arguments match {
          case Nil => ""
          case lst => lst.mkString("[", ", ", "]")
        }
        s"$symbol$argsStr"
      }
    }

    implicit def materialize[A]: TpeTag[A] =
      macro TpeTagMacro.materializeTpeTag[A]
  }

}
