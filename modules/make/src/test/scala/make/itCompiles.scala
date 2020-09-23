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

    @deriveMake
    case class Smth1(a: Int)

    @deriveMake
    case class Smth2(a: Int, b: String)

    @deriveMake
    case class Smth2Implicit[F[_]](a: Int, b: String)(implicit F: Sync[F])

    //implicit def make[MakeEff$macro$5[_]](implicit deps: _root_.make.Make[MakeEff$macro$5, scala.Tuple2[Int, String]], fresh$macro$6: _root_.cats.Applicative[MakeEff$macro$5], x: Double, tag: _root_.make.Tag[Smth2Implicit]): Make[MakeEff$macro$5, Smth2Implicit] = _root_.make.internal.MakeOps.map(deps)(((x0: Int, x1: String) => new Smth2Implicit(x0, x1)).tupled)
    
    @deriveMake
    class NonCase1(a: Int)

    @deriveMake
    class NonCase2(a: Int, b: String)
  }
}