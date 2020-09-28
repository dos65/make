package example

import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import cats.effect.IOApp

import make._
import make.syntax._
import make.cats.effect.implicits._

object Example extends IOApp {

  trait Dep {
    def v: String
  }

  @autoMake
  case class DepImpl(v: String) extends Dep

  trait Logging[F[_]]
  object Logging {
    implicit def inst[F[_]]: Logging[F] = null
  }

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
  case class End[F[_]](yo: Yohoho, yo2: Yohoho2)(implicit val l: Logging[F])

  override def run(args: List[String]): IO[ExitCode] = {
    implicit val depImplAsDep = ContraMake.widen[DepImpl, Dep]

    implicit val initString = Make.value(Resource.pure[IO, String]("asdasd"))

    import enableDebug._
    val make = Make.debugOf[Resource[IO, ?], End[IO]]
    val resource = make.toEff
    val f = for {
       _ <- resource.use(end => IO(println(end)))
    } yield ()
    f.as(ExitCode.Success)
  }

}