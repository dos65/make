package make

import munit.FunSuite
import scala.util.Random
import scala.tools.nsc.Global
import scala.tools.nsc.Settings
import scala.tools.nsc.interactive.tests.core.ConsoleReporter
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.reporters.Reporter
import scala.reflect.internal.util.Position
import scala.reflect.internal.util.ScriptSourceFile
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit

class GenerateTest extends FunSuite {

  // object Namer {
  //   val aIdx = 'A'.toInt
  //   val zIdz = 'Z'.toInt
  //   val range = aIdx - zIdz

  //   def nameFor(i: Int): String = {
  //     val x = i % range
  //     val char = (aIdx + x).toChar
  //     s"$char$i"
  //   }
  // }

  // test("generate") {

  //   val random = new Random
  //   val n = 1000

  //   case class Node(name: String, dependsOn: List[String])

  //   val genTree = (1 to n).foldLeft(List(List.empty[Int])){case (st, n) =>
  //     val depsN = math.min(n, 6)
  //     val depsCount = random.nextInt(depsN)

  //     val candidates = List.tabulate(depsN)(identity)
  //     val toDrop = depsN - depsCount
      
  //     val deps = 
  //       if (toDrop > 0) {
  //         (1 to toDrop).foldLeft(candidates){ case (cd, _) => 
  //           val deleteN = random.nextInt(cd.size)
  //           cd.zipWithIndex.filter(_._2 != deleteN).map(_._1)
  //         }
  //       } else candidates
      
  //     deps :: st
  //   }


  //   val named = genTree.reverse.zipWithIndex.map{case (dependsIdx, i) => Node(Namer.nameFor(i), dependsIdx.map(Namer.nameFor))}

  //   val sb = new StringBuilder
  //   val out = named.foldLeft(new StringBuilder){ case (acc, node) =>
  //     val clazz = if (node.dependsOn.nonEmpty) "case class" else "class"
  //     val params = node.dependsOn.map(name => s"${name.toLowerCase()}:$name").mkString("(", ",", ")")
  //     val makeSection = 
  //       if (node.dependsOn.nonEmpty) {
  //         val depsType = node.dependsOn match {
  //           case single :: Nil => single
  //           case more => more.mkString("(", ",", ")")
  //         }
  //         s"implicit def make(implicit ctx: Make.DepsCtx[IO, $depsType]): Make[IO, ${node.name}] = ctx.funcN(${node.name}.apply)"
  //       } else {
  //         s"implicit def make(implicit ctx: Make.Ctx[IO]): Make[IO, ${node.name}] = ctx.pure(new ${node.name})"
  //       }
  //     val s =
  //       s"""$clazz ${node.name}$params
  //          |object ${node.name} {
  //          |  $makeSection
  //          |}
  //          |""".stripMargin
  //     val s2 =
  //       s"""$clazz ${node.name}$params
  //          |""".stripMargin
  //     acc.append(s2)
  //   }

  //   val fullSource = 
  //     s"""import make._
  //        |import make.syntax._
  //        |import cats.effect._
  //        |""".stripMargin + out.toString

  //   // println(fullSource)

  //   val settings = new Settings()
  //   settings.usejavacp.value = true
  //   println(settings.classpath)
  //   val reporter = new ConsoleReporter(settings)
  //   val compiler = Global(settings, reporter)

  //   val start = System.currentTimeMillis()
  //   val unit = compiler.newCompilationUnit(fullSource)
  //   val source = ScriptSourceFile(unit.source.file, unit.source.content)

  //   val run = new compiler.Run
  //   run.compileSources(source :: Nil)
  //   val stop = System.currentTimeMillis()
  //   println("took:" + Duration.create(stop - start, TimeUnit.MILLISECONDS))
  // }
}