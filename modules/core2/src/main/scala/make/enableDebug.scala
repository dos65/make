package make

import make.internal.MakeMacro

object enableDebug {

  implicit def debugHook[F[_], A]: Debug[MakeDef[F, A]] =
    macro MakeMacro.debugHook[F, A]


  implicit class DebugSyntax(val obj: MakeDef.type) extends AnyVal {
    
    def debugOf[F[_], A]: MakeDef[F, A] = 
      macro MakeMacro.debug[F, A]
  }
}