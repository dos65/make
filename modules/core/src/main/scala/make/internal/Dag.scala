package make.internal

import cats.Monad
import cats.effect.Resource

object Dag {

   case class Entry[F[_]](
     tag: NodeTag[Any],
     create: List[Any] => Resource[F, Any],
     takes: Vector[NodeTag[Any]]
   )

  def toResource[F[_], A](node: Node[A])(implicit F: Monad[F]): Resource[F, A] = {

    def toMap(
      node: Node[Any],
      acc: Map[NodeTag[Any], Entry[F]]
    ): Map[NodeTag[Any], Entry[F]] = {
      node match {
        case Node.Pure(v, tag) =>
          val entry = acc.get(tag) match {
            case None => Entry[F](tag, _ => Resource.pure(v), Vector.empty)
            case Some(e) => e
          }
          acc.updated(tag, entry)
        case Node.Func(prev, f, tag) => 
          val updEntry = acc.get(tag) match {
            case None =>  Entry[F](tag, lst => Resource.pure(f(lst(0))), Vector(prev.tag))
            case Some(e) => e.copy(takes = e.takes :+ prev.tag)
          }
          val next = acc.updated(tag, updEntry)
          toMap(prev, next)
        case Node.Eff(v, tag) => 
          val entry = acc.get(tag) match {
            case None => Entry[F](tag, _ => v.asInstanceOf[Resource[F, Any]], Vector.empty)
            case Some(e) => e
          }
          acc.updated(tag, entry)
        case Node.EffFunc(prev, f, tag) => 
          val updEntry = acc.get(tag) match {
            case None =>  Entry[F](tag, lst => f(lst(0)).asInstanceOf[Resource[F, Any]], Vector(prev.tag))
            case Some(e) => e.copy(takes = e.takes :+ prev.tag)
          }
          val next = acc.updated(tag, updEntry)
          toMap(prev, next)
        case Node.Ap(prev, op, tag) =>
          val opAdded = toMap(op.asInstanceOf[Node[Any]], acc)
          val updEntry = acc.get(tag) match {
            case None =>  Entry[F](
              tag,
              lst => {
                val a = lst(0)
                val aToB = lst(1).asInstanceOf[Any => Any]
                Resource.pure(aToB(a))
              },
              Vector(prev.tag, op.tag.asInstanceOf[NodeTag[Any]])
            )
            case Some(e) => e.copy(takes = e.takes :+ prev.tag)
          }
          val next = opAdded.updated(tag, updEntry)
          toMap(prev, next)
      } 
    }
    val empty = Map.empty[NodeTag[Any], Entry[F]]
    val asMap = toMap(node.asInstanceOf[Node[Any]], empty)

    val indexedKeys = asMap.keys.zipWithIndex.toMap
    val indexedMap = indexedKeys.map {case (tag, _) =>
      val entry = asMap(tag)
      println(entry)
      entry.takes.map(indexedKeys(_)).toList
    }.toList


    val sorted = Tarjans.apply(indexedMap)

    val entriesWithTags = sorted.flatten.map(i => {
      val (tag, idx) = indexedKeys.find(_._2 == i).get
      val entry = asMap(tag)
      (entry, tag)
    })

    
    case class ResourceState(
      map: Map[NodeTag[Any], Any]
    )

    val init = Resource.make[F, Map[NodeTag[Any], Any]](F.pure(Map.empty))(_ => F.unit)
    val resourcePipeline = entriesWithTags.foldLeft(init){ case (accResource, (entry, tag)) =>
      accResource.flatMap(depsMap => {
        val input = entry.takes.map(depsMap(_))
        val rsc = entry.create(input.toList)
        rsc.map(v => depsMap.updated(tag, v)) 
      })
    }

    resourcePipeline.map(all => {
      println(all)
      all(node.tag.asInstanceOf[NodeTag[Any]])
    }).asInstanceOf[Resource[F, A]]
  }

}