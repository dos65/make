package make

import scala.reflect.runtime.universe.Type
import make.Tag.SourcePos

final case class Conflict(tpe: Type, positions: List[SourcePos])