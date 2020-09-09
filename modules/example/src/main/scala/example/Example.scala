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

  @deriveMake
  case class Hoho(dep: Dep)

  @deriveMake
  case class Yohoho(
    dep1: Dep,
    hoho: Hoho
  )

  @deriveMake
  case class Yohoho2(
    dep1: Dep,
    hoho: Hoho
  )

  @deriveMake
  case class End(yo: Yohoho, yo2: Yohoho2)

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