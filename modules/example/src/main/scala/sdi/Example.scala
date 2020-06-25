package sdi

import cats.Monad
import cats.implicits._
import cats.effect.implicits._
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import scala.concurrent.Future

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
      mk.pure(Hoho(_))
  }

  case class Yohoho(
    dep1: Dep1,
    hoho: Hoho 
  )
  object Yohoho {
    implicit def make(implicit mk: Make.DepsCtx[IO, (Dep1, Hoho)]): Make[IO, Yohoho] =
      mk.pure{ case (x, y) => Yohoho(x, y) }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    implicit val ctx = Make.context[IO]

    val resource = Make.inCtx[IO].derive[Yohoho].asResource
    val f = for {
      _ <- resource.use(v => IO(println(v)))
    } yield ()

    f.as(ExitCode.Success)
  }

}