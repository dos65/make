package example

import zio._
import zio.console._
import make._
import make.syntax._

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

  override def run(args: List[String]): URIO[ZEnv, ExitCode] = {

    // For ZIO it's required to define MakeEff TC manually to specify R, E
    type MakeZManaged[A] = ZManaged[Console, Nothing, A]
    implicit val zmanagedEff =
      new MakeEff[MakeZManaged] {
        def map[A, B](fa: MakeZManaged[A])(f: A => B): MakeZManaged[B] = fa.map(f)
        def pure[A](a: A): MakeZManaged[A] = ZManaged.effectTotal(a)
        def flatMap[A, B](fa: MakeZManaged[A])(f: A => MakeZManaged[B]): MakeZManaged[B] =
          fa.flatMap(f)
      }

    implicit val depImplAsDep = ContraMake.widen[DepImpl, Dep]
    implicit val initString = Make.eff[MakeZManaged, String](
      ZManaged.make(putStrLn("Yoyo") *> ZIO.effectTotal("asd"))(_ => putStrLn("Close"))
    )

    import enableDebug._
    val value = Make.debugOf[MakeZManaged, End]

    for {
      graph <- ZIO.fromEither(value.toGraph).orDie
      zmanaged = graph.initEff
      _ <- zmanaged.use(r => putStrLn(r.toString))
    } yield ExitCode.success
  }
}
