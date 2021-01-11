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
    state.stack.find(_ == atpe.tpe) match {
      case None => 
        state.stack.prepend(atpe.tpe)
        state.cache.get(atpe.tpe).getOrElse(state.infer[F](ftpe.tpe, atpe.tpe)) match {
          case EmptyTree =>
            c.abort(c.enclosingPosition, "failed")
          case tree =>
            state.stack.remove(0)
            state.cache.update(atpe.tpe, tree)
            c.Expr[Dep[F, A]](q"_root_.make.Dep[${ftpe.tpe}, ${atpe.tpe}]($tree)")
        }
      case Some(_) =>
        // TODO
        println(s"-----CYCLE!!!! ${atpe} ${state.stack}")
        c.abort(c.enclosingPosition, "Cycle detected")
    }
  }

  private def getOrCreateState: State = {
    val existing = 
      c.openMacros.find(c => c.internal.attachments(c.macroApplication).contains[State])
        .flatMap(c => c.internal.attachments(c.macroApplication).get[State])
    existing match {
      case None =>
        val st = new State(mutable.ListBuffer.empty, mutable.HashMap.empty)
        c.internal.updateAttachment(c.macroApplication, st)
        st
      case Some(st) => st
    }
  }


  class State(
    val stack: mutable.ListBuffer[Type],
    val cache: mutable.HashMap[Type, Tree]
  ) {

    def infer[F[_]](ftpe: c.Type, atpe: c.Type): c.Tree = {
      val makeTc = c.universe.weakTypeOf[Make[F, _]].typeConstructor
      val searchType = c.universe.appliedType(makeTc, ftpe, atpe)
      c.inferImplicitValue(searchType)
    }
  }
}