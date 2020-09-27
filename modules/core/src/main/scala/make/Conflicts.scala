package make

import scala.reflect.runtime.universe.Type
import make.Tag.SourcePos

final case class Conflicts(values: List[Conflicts.TpeConflict])
object Conflicts {
  final case class TpeConflict(tpe: Type, positions: List[SourcePos])
}