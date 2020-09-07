package make.internal

import scala.collection.mutable

// Taken from https://github.com/lihaoyi/mill/blob/master/main/core/src/eval/Tarjans.scala
object Tarjans {
  def apply(graph0: TraversableOnce[TraversableOnce[Int]]): Seq[Seq[Int]] = {
    val graph = graph0.map(_.toArray).toArray
    val n = graph.length
    val visited = new Array[Boolean](n)
    val stack = mutable.ArrayBuffer.empty[Integer]
    var time = 0
    val lowlink = new Array[Int](n)
    val components = mutable.ArrayBuffer.empty[Seq[Int]]
    
    
    for (u <- 0 until n) {
      if (!visited(u)) dfs(u)
    }
    
    def dfs(u: Int): Unit = {
      lowlink(u) = time
      time += 1
      visited(u) = true
      stack.append(u)
      var isComponentRoot = true
      for (v <- graph(u)) {
        if (!visited(v)) dfs(v)
        if (lowlink(u) > lowlink(v)) {
          lowlink(u) = lowlink(v)
          isComponentRoot = false
        }
      }
      if (isComponentRoot) {
        val component = mutable.Buffer.empty[Int]
        
        var done = false
        while (!done) {
          val x = stack.last
          stack.remove(stack.length - 1)
          component.append(x)
          lowlink(x) = Integer.MAX_VALUE
          if (x == u) done = true
        }
        components.append(component.toSeq)
      }
    }
    components.toSeq
  }
}

