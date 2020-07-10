package example

import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import cats.effect.IOApp

import make._

object Example extends IOApp {

  case class Dep1(v: String)
  object Dep1 {
    implicit def make(implicit mk: Make.Ctx[IO]): Make[IO, Dep1] =
      mk.effect(IO.pure(Dep1("asdasdas")))
  }

  case class Hoho(dep1: Dep1)
  object Hoho {
    implicit def make(implicit mk: Make.DepsCtx[IO, Dep1]): Make[IO, Hoho] =
      mk.func(Hoho(_))
  }

  case class Yohoho(
    dep1: Dep1,
    hoho: Hoho 
  )
  object Yohoho {
    implicit def make(implicit mk: Make.DepsCtx[IO, (Dep1, Hoho)]): Make[IO, Yohoho] =
      mk.func{ case (x, y) => Yohoho(x, y) }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    implicit val ctx = Make.context[IO]

    implicit val hohoOverride: Make[IO, Hoho] = ctx.pure(Hoho(Dep1("override")))

    val resource = Make.inCtx[IO].derive[Yohoho].asResource
    val f = for {
      _ <- resource.use(v => IO(println(v)))
    } yield ()

    f.as(ExitCode.Success)
  }

}