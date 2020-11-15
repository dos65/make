package make

object annotated {

  case class :@:[+A, B](value: A)

  implicit class Syntax[A](val a: A) extends AnyVal {
    def annotated[B]: A :@: B = :@:(a)
  }
}