package make

import cats.effect.Resource
import make.internal.MakeOps
import cats.effect.IO
import cats.effect.Sync

/**
  * Not an actual test - just checks that code compiles
  */
object itCompiles {

  object annotations {

    @autoMake
    case class Smth1(a: Int)
    object Smth1Test {
      implicit val intMake = Make.pure[IO, Int](42)
      Make.of[IO, Smth1]
    }

    @autoMake
    case class Smth2(a: Int, b: String)
    object Smth2Test {
      implicit val intMake = Make.pure[IO, Int](42)
      implicit val stringMake = Make.pure[IO, String]("42")
      Make.of[IO, Smth2]
    }

    @autoMake
    case class Smth2Implicit[F[_]](a: Int, b: String)(implicit val F: Sync[F])
    object Smth2ImplicitTest {
      implicit val intMake = Make.pure[IO, Int](42)
      implicit val stringMake = Make.pure[IO, String]("42")
      Make.of[IO, Smth2Implicit[IO]]
    }

    @autoMake
    class NonCase1(a: Int)
    object NonCase1Test {
      implicit val intMake = Make.pure[IO, Int](42)
      Make.of[IO, NonCase1]
    }

    @autoMake
    class NonCase2(a: Int, b: String)
    object NonCase2Test {
      implicit val intMake = Make.pure[IO, Int](42)
      implicit val stringMake = Make.pure[IO, String]("42")
      Make.of[IO, NonCase2]
    }

    @autoMake
    class NonCaseImplicit[F[_]](a: Int, b: String)(implicit val F: Sync[F])
    object NonCaseImplicitTest {
      implicit val intMake = Make.pure[IO, Int](42)
      implicit val stringMake = Make.pure[IO, String]("42")
      Make.of[IO, NonCaseImplicit[IO]]
    }

    @autoMake
    class NonCaseImplicit2[F[_]: Sync](a: Int, b: String)
    object NonCaseImplicit2Test {
      implicit val intMake = Make.pure[IO, Int](42)
      implicit val stringMake = Make.pure[IO, String]("42")
      Make.of[IO, NonCaseImplicit2[IO]]
    }
  }

}
