package example

import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import cats.effect.IOApp

import make._
//import make.enableDebug._
import make.syntax._
import cats.Applicative

object Example extends IOApp {

  trait Dep {
    def v: String
  }
  case class DepImpl(v: String) extends Dep

  case class Hoho(dep: Dep)
  object Hoho {
    implicit def make[F[_]: Applicative](implicit depM: Make[F, Dep]): Make[F, Hoho] =
      depM.map(Hoho(_))
  }

  case class Yohoho(
    dep1: Dep,
    hoho: Hoho
  )

  object Yohoho {
    implicit def make[F[_]: Applicative](implicit deps: Make[F, (Dep, Hoho)]) =
      deps.mapN(Yohoho.apply)
  }

  case class Yohoho2(
    dep1: Dep,
    hoho: Hoho
  )

  object Yohoho2 {
    // implicit def make[F[_]: Applicative](implicit deps: Make[F, (Dep, Hoho)]) = {
    //   val wrong = (DepImpl("asd"), Hoho(DepImpl("gogo"))) 
    //   Make.pure[F, (Dep, Hoho)](wrong)
    //     .mapN(Yohoho2.apply)
    // }
    implicit def make[F[_]: Applicative](implicit deps: Make[F, (Dep, Hoho)]) =
      deps.mapN(Yohoho2.apply)
  }

  case class End(yo: Yohoho, yo2: Yohoho2)
  object End {
    implicit def make[F[_]: Applicative](implicit deps: Make[F, (Yohoho, Yohoho2)]) =
      deps.mapN(End.apply)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    implicit val depMake = Make.pure[IO, Dep](new DepImpl("asdad"))

    val make = Make.of[IO, End]
    val resource = make.toResource
    val f = for {
       _ <- resource.use(end => IO(println(end)))
    } yield ()
    f.as(ExitCode.Success)
  }
}