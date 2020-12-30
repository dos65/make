package make

import make.internal.DepMacro

/**
  * This class is used to workaround wrong `divergency implicit error` on scala 2.12
  * Macros uses similar trick as `shapeless.Lazy` but do not allows recursive references 
  */
case class Dep[F[_], A](value: Make[F, A])

object Dep {

  implicit def materialize[F[_], A]: Dep[F, A] = 
    macro DepMacro.materialize[F, A]
}