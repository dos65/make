package make.internal

import scala.reflect.macros.whitebox
import scala.collection.mutable
import make.Dep
import make.Make
import scala.annotation.tailrec

class DepMacro(val c: whitebox.Context) {

  import c.universe._

  def materialize[F[_], A](implicit
    ftpe: WeakTypeTag[F[X] forSome { type X }],
    atpe: WeakTypeTag[A]
  ): c.Expr[Dep[F, A]] = {
    val (root, state) = getOrCreateState
    val out = state.stack.find(_ =:= atpe.tpe) match {
      case None => 
        state.stack.append(atpe.tpe)
        state.cache.get(atpe.tpe).getOrElse(state.infer[F](ftpe.tpe, atpe.tpe)) match {
          case EmptyTree =>
            state.cache.update(atpe.tpe, EmptyTree)
            c.abort(c.enclosingPosition, "failed")
          case tree =>
            val last = state.stack.length
            val depTree = q"_root_.make.Dep[${ftpe.tpe}, ${atpe.tpe}]($tree)"

            state.stack.remove(last - 1)
            state.cache.update(atpe.tpe, tree)
            c.Expr[Dep[F, A]](depTree)
        }
      case Some(_) =>
        state.cache.update(atpe.tpe, EmptyTree)
        c.abort(c.enclosingPosition, s"Cycle detected ${atpe.tpe}: ${state.stack.mkString("\n")}")
    }
    
    if (root) {
      //println(state.cache.mkString("\n"))
      c.internal.removeAttachment[State](c.macroApplication)

      val clsName = TypeName(c.freshName("Dep"))

      val named = state.cache.map{case (tpe, instance) =>
        val newName = TermName(c.freshName("instance"))
        val depTc = c.universe.weakTypeOf[Make[F, _]].typeConstructor
        val fullTpe = c.universe.appliedType(depTc, ftpe.tpe, tpe)
        (tpe, (newName, instance))
      }

      val (from, to) = named.map{case (_, (name, instance)) => 
        (instance.symbol, name)
      }.unzip

      val substituted = named.map{case (tpe, (name, instance)) =>
        val (from, to) = named.filter(_._2._1 != name)
          .map{case (tpe, (name, instance)) => 
            val depTc = c.universe.weakTypeOf[Make[F, _]].typeConstructor
            val fullTpe = c.universe.appliedType(depTc, ftpe.tpe, tpe)
            ((instance.symbol, fullTpe), (name, fullTpe))
          }.unzip
        val replaced = new TransformerX(clsName, from.toList, to.toList).transform(instance)
        val z = new StripUnApplyNodes().transform(c.untypecheck(replaced))
        (tpe, (name, replaced))
      }

      val instances = substituted.map{ case (tpe, (name, instance)) => 
        q"def $name: _root_.make.Make[${ftpe.tpe}, $tpe] = $instance"
      }

      val primaryName = named(atpe.tpe)._1
      val tree = 
        q"""
            final class $clsName {
               ..$instances
            }
            _root_.make.Dep.apply[${ftpe.tpe}, ${atpe.tpe}]((new $clsName).$primaryName)
        """
      //pprint.pprintln(tree, 100, Int.MaxValue)
      c.Expr[Dep[F, A]](tree)
    } else {
      out
    }
  }

  private def getOrCreateState: (Boolean, State) = {
    val existing = 
      c.openMacros.find(c => c.internal.attachments(c.macroApplication).contains[State])
        .flatMap(c => c.internal.attachments(c.macroApplication).get[State])
    existing match {
      case None =>
        val st = new State(mutable.ArrayBuffer.empty, mutable.HashMap.empty)
        c.internal.updateAttachment(c.macroApplication, st)
        (true, st)
      case Some(st) =>
        (false, st)
    }
  }


  class State(
    val stack: mutable.ArrayBuffer[Type],
    val cache: mutable.HashMap[Type, Tree]
  ) {

    def infer[F[_]](ftpe: c.Type, atpe: c.Type): c.Tree = {
      val makeTc = c.universe.weakTypeOf[Make[F, _]].typeConstructor
      val searchType = c.universe.appliedType(makeTc, ftpe, atpe)
      c.inferImplicitValue(searchType)
    }
  }

  class TransformerX(
    className: TypeName,
    fromSymbols: List[(Symbol, Type)],
    toSymbols: List[(TermName, Type)],
  ) extends Transformer {

    @tailrec
    private def findReplacement(
      sym: Symbol,
      tpe: Type,
      from: List[(Symbol, Type)],
      to: List[(TermName, Type)]
    ): Option[(TermName, Type)] = {
      from match {
        case (head, otpe) :: tl =>
          if (sym == head && tpe == otpe){
            Some(to.head)
          } else findReplacement(sym, tpe, tl, to.tail)
        case Nil => None
      }
    }

    override def transform(tree: c.universe.Tree): c.universe.Tree = {
      tree match {
        case v @ Apply(tA @ TypeApply(fn, params), other) =>
           findReplacement(fn.symbol, tA.tpe.finalResultType, fromSymbols, toSymbols) match {
             case Some((sym, tpe)) =>
               Select(This(className), sym)
             case None =>
              Apply(TypeApply(transform(fn), transformTrees(params)), transformTrees(other))
           } 
        case a @ Apply(fn, params) =>
           findReplacement(fn.symbol, a.tpe.finalResultType, fromSymbols, toSymbols) match {
             case Some((sym, tpe)) =>
               Select(This(className), sym)
             case None =>
              Apply(transform(fn), transformTrees(params))
           } 
        case x => x
      }
    }

  }

  class StripUnApplyNodes extends Transformer {
    val global = c.universe.asInstanceOf[scala.tools.nsc.Global]
    import global.nme

    override def transform(tree: Tree): Tree = {
      super.transform {
        tree match {
          case UnApply(Apply(Select(qual, nme.unapply | nme.unapplySeq), List(Ident(nme.SELECTOR_DUMMY))), args) =>
            Apply(transform(qual), transformTrees(args))
          case UnApply(Apply(TypeApply(Select(qual, nme.unapply | nme.unapplySeq), _), List(Ident(nme.SELECTOR_DUMMY))), args) =>
            Apply(transform(qual), transformTrees(args))
          case t => t
        }
      }
    }
  }

}
