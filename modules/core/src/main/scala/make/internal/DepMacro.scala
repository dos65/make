package make.internal

import scala.reflect.macros.whitebox
import scala.collection.mutable
import make.Dep
import make.Make

class DepMacro(val c: whitebox.Context) {

  import c.universe._

  def materialize[F[_], A](implicit
    ftpe: WeakTypeTag[F[X] forSome { type X }],
    atpe: WeakTypeTag[A]
  ): c.Expr[Dep[F, A]] = {
    val state = getOrCreateState
    state.infer[F](ftpe.tpe, atpe.tpe) match {
      case EmptyTree =>
        c.abort(c.enclosingPosition, "failed")
      case tree =>
        c.Expr[Dep[F, A]](q"_root_.make.Dep[${ftpe.tpe}, ${atpe.tpe}]($tree)")
    }
  }

  private def getOrCreateState: State = {
    val existing = 
      c.openMacros.find(c => c.internal.attachments(c.macroApplication).contains[State])
        .flatMap(c => c.internal.attachments(c.macroApplication).get[State])
    existing match {
      case None =>
        val st = new State(mutable.HashSet.empty)
        c.internal.updateAttachment(c.macroApplication, st)
        st
      case Some(st) => st
    }
  }


  class State(val seen: mutable.HashSet[Type]) {

    def infer[F[_]](ftpe: c.Type, atpe: c.Type): c.Tree = {
      val makeTc = c.universe.weakTypeOf[Make[F, _]].typeConstructor
      val searchType = c.universe.appliedType(makeTc, ftpe, atpe)
      c.inferImplicitValue(searchType)
    }
  }
}