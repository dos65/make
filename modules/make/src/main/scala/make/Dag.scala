package make

import scala.reflect.runtime.universe.Type
import cats.effect.Resource
import make.Make.Value
import make.Make.Bind
import make.Make.Ap
import scala.annotation.tailrec
import cats.Applicative
import make.Tag.SourcePos
import cats.data.NonEmptyList
import cats.implicits._
import make.internal.Tarjans

final class Dag[F[_], A](
  entries: Map[Type, Dag.RawEntry[F]],
  targetTpe: Type
)(implicit F: Applicative[F]) {

  def toResource: Resource[F, A] = {
    val order = initOrder

    val init = Resource.make[F, Map[Type, Any]](F.pure(Map.empty))(_ => F.unit)

    val rs = order.foldLeft(init){ case (rs, tpe) =>
      rs.flatMap(depsMap => {

        val entry = entries(tpe)
        val input = entry.dependsOn.map(depsMap(_))
        val rsc = entry.f(input.toList)

        rsc.map(v => depsMap.updated(tpe, v)) 
      })
    }
    
    rs.map(values => values(targetTpe).asInstanceOf[A])
  }

  private def initOrder: List[Type] = {
    val indexedKeys = entries.keys.zipWithIndex.toMap
    val indexedMap = indexedKeys.map {case (tpe, _) =>
      val entry = entries(tpe)
      entry.dependsOn.map(indexedKeys(_)).toList
    }.toList
    val sorted = Tarjans.apply(indexedMap)

    sorted.flatten.map(i => {
      val (tpe, idx) = indexedKeys.find(_._2 == i).get
      tpe
    }).toList
  }

}

object Dag {

  case class RawEntry[F[_]](
    tpe: Type,
    pos: Tag.SourcePos,
    dependsOn: List[Type],
    f: List[Any] => Resource[F, Any]
  )

  def fromMake[F[_]: Applicative, A](v: Make[F, A]): Either[NonEmptyList[Conflict], Dag[F, A]] = {
    val allEntriesMap = makeToAllEntriesMap(
      Map.empty,
      List(v.asInstanceOf[Make[F, Any]])
    )

    val init = (Map.empty[Type, RawEntry[F]], List.empty[Conflict])
    val (okMap, errors) =
      allEntriesMap.foldLeft(init){
        case ((okAcc, errAcc), (tpe, entries)) =>
          val refs = entries.foldLeft(Set.empty[SourcePos]){case (acc, e) => acc + e.pos }
          if (refs.size > 1) {
            val error = Conflict(tpe, refs.toList)
            (okAcc, error :: errAcc)
          } else {
            val nextOk = okAcc.updated(tpe, entries.head)
            (nextOk, errAcc)
          }
      }
    
    NonEmptyList.fromList(errors) match {
      case Some(nel) => nel.asLeft
      case None => new Dag(okMap, v.tag.typeTag.tpe).asRight
    }
  }

  @tailrec
  private def makeToAllEntriesMap[F[_]: Applicative](
    acc: Map[Type, List[RawEntry[F]]],
    stack: List[Make[F, Any]]
  ): Map[Type, List[RawEntry[F]]] = {

    type HandleOut = (List[Any] => Resource[F, Any], List[Type], List[Make[F, Any]])

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
            Resource.pure[F, Any](aToB(a))
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