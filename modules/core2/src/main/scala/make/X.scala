package make

trait X[F[_], A] {
  type Trace <: X.TraceType
}
object X {

  sealed trait TraceType
  object TraceType {
    sealed trait Ok extends TraceType
    object Ok extends TraceType

    sealed trait Err extends TraceType

    sealed trait ErrNil extends Err
    final case object ErrNil extends ErrNil
    final class ErrCons[A]() extends Err
  }


  sealed trait Def[F[_], A] extends X[F, A] {
    type Trace = X.TraceType.Ok
  }

  sealed trait Fail[F[_], A] extends X[F, A] {
    type Trace <: X.TraceType.Err
  }
}