package example

import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import cats.effect.IOApp

import make._
import make.syntax._
import cats.Applicative
// import make.ce.resource._

object ExampleCatsEffect extends IOApp {

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

  case class A(b: B)
  object A {
    implicit def make[F[_]: Applicative](implicit bM: Strict[Make[F, B]]): Make[F, A] = 
      bM.value.map(new A(_)) 
  }
  case class B(a: A)
  object B {
    implicit def make[F[_]: Applicative](implicit aM: Strict[Make[F, A]]): Make[F, B] = 
      aM.value.map(new B(_)) 
  }

  @autoMake
  case class Z(v: String, a: A, b: B)

  override def run(args: List[String]): IO[ExitCode] = {
    implicit val depImplAsDep = Make.widen[Dep, DepImpl]

    type InitEff[A] = Resource[IO, A]
    implicit val initString = Make.eff(Resource.pure[IO, String]("asdasd"))

    import enableDebug._
    val make = Make.debugOf[InitEff, Z]

    for {
      _ <- make.make.use(end => IO(println(end)))
    } yield ExitCode.Success
  }

}
