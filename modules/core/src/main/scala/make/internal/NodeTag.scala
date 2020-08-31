package make.internal

import scala.reflect.ClassTag

case class NodeTag[A](
  classTag: Class[_],
  pos: NodeTag.Pos
)

object NodeTag {

  case class Pos(
    source: sourcecode.File,
    line: sourcecode.Line,
    enclosing: sourcecode.Enclosing
  )
  object Pos {
    implicit def pos(
      implicit
        file: sourcecode.File,
        line: sourcecode.Line,
        enclosing: sourcecode.Enclosing
     ): Pos = Pos(file, line, enclosing)
  }

  implicit def tag[A](
    implicit 
      file: sourcecode.File,
      line: sourcecode.Line,
      enclosing: sourcecode.Enclosing,
      tag: ClassTag[A]
  ): NodeTag[A] =
    NodeTag(tag.runtimeClass, Pos(file, line, enclosing))

  def of[A](implicit v: NodeTag[A]): NodeTag[A] = v
}