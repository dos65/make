package make

import scala.reflect.runtime.universe.TypeTag
import scala.reflect.runtime.universe.WeakTypeTag
import make.internal.SourcePosMacro
import make.internal.TpeTagMacro

final case class Tag[A](
  typeTag: WeakTypeTag[A],
  sourcePos: Tag.SourcePos,
  test: Tag.TpeTag[A]
)

object Tag {

  def of[A](implicit tag: Tag[A]): Tag[A] = tag

  implicit def tagFor[A](implicit typeTag: WeakTypeTag[A], tpeTag:TpeTag[A], sourcePos: SourcePos): Tag[A] =
    Tag(typeTag, sourcePos, tpeTag)

  final case class SourcePos(path: String, line: Int, start: Int)
  object SourcePos {
    implicit def materialize: SourcePos =
      macro SourcePosMacro.materializeSourcePos
  }

  final case class TpeTag[A](tpe: TpeTag.Type)

  object TpeTag {

    sealed trait Type {
      def isFullyResolved: Boolean
    }
    object Type {
      final case class Parameter(name: String, arguments: List[Type]) extends Type {
        def isFullyResolved: Boolean = false
      }
      final case class Stable(symbol: String, arguments: List[Type]) extends Type {
        def isFullyResolved: Boolean = arguments.forall(_.isFullyResolved)
      }
    }

    def apply[A](implicit tag: TpeTag[A]): TpeTag[A] = tag

    implicit def materialize[A]: TpeTag[A] =
      macro TpeTagMacro.materializeTpeTag[A]
  }
}
