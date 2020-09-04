package make

import make.internal.MakeMacro

object enableDebug {

  implicit def debugHook[F[_], A]: Debug[Make[F, A]] =
    macro MakeMacro.debugHook[F, A]


  implicit class DebugSyntax(val obj: Make.type) extends AnyVal {
    
    def debugOf[F[_], A]: Make[F, A] = 
      macro MakeMacro.debug[F, A]
  }
}