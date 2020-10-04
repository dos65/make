package make

import scala.reflect.runtime.universe.Type
import make.Tag.SourcePos

final case class Conflicts(values: List[Conflicts.TpeConflict]) extends Exception {
  override def getMessage(): String = {
    def renderSingle(v: Conflicts.TpeConflict): String =
      s"${v.tpe} defined at ${v.positions.mkString(",")}"
    s"Conflicts: ${values.map(renderSingle).mkString("; ")}"
  }
}
object Conflicts {
  final case class TpeConflict(tpe: Type, positions: List[SourcePos])
}
