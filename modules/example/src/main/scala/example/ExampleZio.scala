package example

import zio._
import zio.console._
import make._
import make.syntax._
import make.zio.implicits._

object ExampleZio extends App {

  trait Dep {
    def v: String
  }

  @autoMake
  case class DepImpl(v: String) extends Dep

  @autoMake
  case class Hoho(dep: Dep)

  @autoMake
  case class Yohoho(
    dep1: Dep,
    hoho: Hoho
  )

  @autoMake
  class Yohoho2(
    dep1: Dep,
    hoho: Hoho
  )

  @autoMake
  class End(yo: Yohoho, yo2: Yohoho2)

  override def run(args: List[String]): URIO[ZEnv,ExitCode] = {
     implicit val depImplAsDep = ContraMake.widen[DepImpl, Dep]
     implicit val initString = Make.eff(ZManaged.effect("asdasd"))

     val value = Make.of[ZManaged[Any, Throwable, ?], End]
     val managed = value.toEff
     for {
       _ <- managed.orDie.use(r => putStrLn(r.toString))
     } yield ExitCode.success
  }
}