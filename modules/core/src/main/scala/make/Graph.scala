package make

import scala.reflect.runtime.universe.Type
import make.Make.Value
import make.Make.Bind
import make.Make.Ap
import scala.annotation.tailrec
import make.Tag.SourcePos
import make.internal.Tarjans
import cats.Monad
import cats.Applicative

final class Graph[F[_], A](
  entries: Map[Type, Graph.RawEntry[F]],
  targetTpe: Type
)(implicit F: Monad[F]) {

  def initEff: F[A] = {
    val order = initOrder
    val init = F.pure(Map.empty[Type, Any])

    val rs = order.foldLeft(init) { case (rs, tpe) =>
      F.flatMap(rs)(depsMap => {

        val entry = entries(tpe)
        val input = entry.dependsOn.map(depsMap(_))
        val rsc = entry.f(input.toList)
        F.map(rsc)(v => depsMap.updated(tpe, v))
      })
    }

    F.map(rs)(values => values(targetTpe).asInstanceOf[A])
  }

  private def initOrder: List[Type] = {
    val indexedKeys = entries.keys.zipWithIndex.toMap
    val indexedMap = indexedKeys.map { case (tpe, _) =>
      val entry = entries(tpe)
      entry.dependsOn.map(indexedKeys(_)).toList
    }.toList
    val sorted = Tarjans.apply(indexedMap)

    sorted.flatten
      .map(i => {
        val (tpe, idx) = indexedKeys.find(_._2 == i).get
        tpe
      })
      .toList
  }

}

object Graph {

  case class RawEntry[F[_]](
    tpe: Type,
    pos: Tag.SourcePos,
    dependsOn: List[Type],
    f: List[Any] => F[Any]
  )

  def fromMake[F[_]: Monad, A](v: Make[F, A]): Either[Conflicts, Graph[F, A]] = {
    val allEntriesMap = makeToAllEntriesMap(
      Map.empty,
      List(v.asInstanceOf[Make[F, Any]])
    )

    val init = (Map.empty[Type, RawEntry[F]], List.empty[Conflicts.TpeConflict])
    val (okMap, errors) =
      allEntriesMap.foldLeft(init) { case ((okAcc, errAcc), (tpe, entries)) =>
        val refs = entries.foldLeft(Set.empty[SourcePos]) { case (acc, e) => acc + e.pos }
        if (refs.size > 1) {
          val error = Conflicts.TpeConflict(tpe, refs.toList)
          (okAcc, error :: errAcc)
        } else {
          val nextOk = okAcc.updated(tpe, entries.head)
          (nextOk, errAcc)
        }
      }

    if (errors.size > 0) {
      Left(Conflicts(errors))
    } else {
      Right(new Graph(okMap, v.tag.typeTag.tpe))
    }
  }

  @tailrec
  private def makeToAllEntriesMap[F[_]: Applicative](
    acc: Map[Type, List[RawEntry[F]]],
    stack: List[Make[F, Any]]
  ): Map[Type, List[RawEntry[F]]] = {

    type HandleOut = (List[Any] => F[Any], List[Type], List[Make[F, Any]])

    def handleNode(v: Make[F, Any]): HandleOut = v match {
      case Make.Value(v, tag) =>
        ((_: List[Any]) => v, List.empty, List.empty)
      case Make.Bind(prev, f, tag) =>
        val func = (in: List[Any]) => f(in(0))
        val deps = List(prev.tag.typeTag.tpe)
        val other = List(prev)
        (func, deps, other)
      case Make.Ap(prev, op, tag) =>
        val func =
          (in: List[Any]) => {
            val a = in(0)
            val aToB = in(1).asInstanceOf[Any => Any]
            Applicative[F].pure[Any](aToB(a))
          }
        val deps = List(
          prev.tag.typeTag.tpe,
          op.tag.typeTag.tpe
        )
        val other = List(
          prev,
          op.asInstanceOf[Make[F, Any]]
        )
        (func, deps, other)
    }

    def handleMake(make: Make[F, Any]): (RawEntry[F], List[Make[F, Any]]) = {
      val tpe = make.tag.typeTag.tpe
      val (f, deps, toStack) = handleNode(make)
      val entry = RawEntry[F](
        tpe,
        make.tag.sourcePos,
        deps,
        f
      )
      (entry, toStack)
    }

    stack match {
      case Nil => acc
      case mk :: tail =>
        val tpe = mk.tag.typeTag.tpe
        val (entry, toStack) = handleMake(mk)

        val lst = acc.getOrElse(tpe, List.empty)
        val updLst = entry :: lst
        val nextAcc = acc.updated(tpe, updLst)
        val nextStack = toStack ++ stack.tail
        makeToAllEntriesMap(nextAcc, nextStack)
    }
  }
}
