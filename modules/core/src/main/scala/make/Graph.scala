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
  entries: Map[Graph.Id, Graph.RawEntry[F]],
  targetId: Graph.Id
)(implicit F: Monad[F]) {

  def x = entries

  def initEff: F[A] = {
    val order = initOrder
    val init = F.pure(Map.empty[Graph.Id, Any])

    val rs = order.foldLeft(init) { case (rs, id) =>
      F.flatMap(rs)(depsMap => {

        val entry = entries(id)
        val input = entry.dependsOn.map(depsMap(_))
        val rsc = entry.f(input.toList)
        F.map(rsc)(v => depsMap.updated(id, v))
      })
    }

    F.map(rs)(values => values(targetId).asInstanceOf[A])
  }

  private def initOrder: List[Graph.Id] = {
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

  case class Id(tpe: Tag.TpeTag.Type, pos: Tag.SourcePos)
  object Id {
    def fromTag[A](tag: Tag[A]): Id = 
      Id(tag.typeTag.tpe, tag.sourcePos)
  }

  case class RawEntry[F[_]](
    id: Id,
    dependsOn: List[Id],
    f: List[Any] => F[Any]
  )

  def fromMake[F[_]: Monad, A](v: Make[F, A]): Graph[F, A] = {
    val allEntriesMap = makeToAllEntriesMap(
      Map.empty,
      List(v.asInstanceOf[Make[F, Any]])
    )
    new Graph(allEntriesMap, Id.fromTag(v.tag))
  }

  @tailrec
  private def makeToAllEntriesMap[F[_]: Applicative](
    acc: Map[Id, RawEntry[F]],
    stack: List[Make[F, Any]]
  ): Map[Id, RawEntry[F]] = {

    type HandleOut = (List[Any] => F[Any], List[Id], List[Make[F, Any]])

    def handleNode(v: Make[F, Any]): HandleOut = v match {
      case Make.Value(v, tag) =>
        ((_: List[Any]) => v, List.empty, List.empty)
      case Make.Bind(prev, f, tag) =>
        val func = (in: List[Any]) => f(in(0))
        val deps = List(Id.fromTag(prev.tag))
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
          Id.fromTag(prev.tag),
          Id.fromTag(op.tag)
        )
        val other = List(
          prev,
          op.asInstanceOf[Make[F, Any]]
        )
        (func, deps, other)
    }

    def handleMake(make: Make[F, Any]): (RawEntry[F], List[Make[F, Any]]) = {
      val id = Id(make.tag.typeTag.tpe, make.tag.sourcePos)
      val (f, deps, toStack) = handleNode(make)
      val entry = RawEntry[F](
        id,
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

        val key = Id.fromTag(mk.tag)
        val nextAcc = acc.updated(key, entry)
        val nextStack = toStack ++ stack.tail
        makeToAllEntriesMap(nextAcc, nextStack)
    }
  }
}
