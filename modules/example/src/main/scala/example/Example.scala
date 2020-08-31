package example

import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import cats.effect.IOApp

import make._

object Example extends IOApp {

  trait Dep {
    def v: String
  }
  case class DepImpl(v: String) extends Dep

  case class Hoho(dep: Dep)
  object Hoho {
    implicit def make(implicit mk: Make.DepsCtx[IO, Dep]): Make[IO, Hoho] =
      mk.func(Hoho(_))
  }

  case class Yohoho(
    dep1: Dep,
    hoho: Hoho 
  )
  object Yohoho {
    implicit def make(implicit mk: Make.DepsCtx[IO, (Dep, Hoho)]): Make[IO, Yohoho] =
      mk.func{ case (x, y) => Yohoho(x, y) }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    implicit val ctx = Make.context[IO]

    //implicit val hohoOverride: Make[IO, Hoho] = ctx.pure(Hoho(DepImpl("override")))
    implicit def make(implicit mk: Make.Ctx[IO]): Make[IO, Dep] =
      mk.effect(IO.pure(DepImpl("asdasdas")))
    
    val resource = Make.inCtx[IO].derive[Yohoho].asResource
    val f = for {
      _ <- resource.use(v => IO(println(v)))
    } yield ()

   f.as(ExitCode.Success)
  }

}